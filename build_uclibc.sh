#!/bin/sh

#	SCRIPT TO BUILD ARC-LINUX-UCLIBC TOOLKIT
#	----------------------------------------

# Invocation Syntax
#	build_uclibc.sh	<INSTALLDIR> <LINUXDIR>

# INSTALLDIR = Directory where arc-linux-uclibc- toolkit will be installed.
# LINUXDIR = Directory where ARC Linux sources have been placed.

INSTALLDIR=$1
LINUXDIR=$2

if [ "$INSTALLDIR" = "" ]; then
 echo Usage: build_uclibc.sh  INSTALLDIR  LINUXDIR
fi
if [ "$LINUXDIR" = "" ]; then
 echo Usage: build_uclibc.sh  INSTALLDIR  LINUXDIR
fi

echo "#build binutils" >> uclibc_log.txt
echo "#-------------------------------------------------------------------------------------------------------------" >> uclibc_log.txt
cd binutils/build
rm -r -f *
echo "Start building BINUTILS!"
echo "..."
../src/configure --prefix=$INSTALLDIR --target=arc-linux-uclibc --disable-werror > ../../uclibc_log.txt 2>> ../../uclibc_log.txt

if make >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt ; then
 echo "Finish building BINUTILS!"
else
 echo [build_uclibc.sh] Fatal Error: BINUTILS build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ../..
 exit 1
fi
make install >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
cd ../..
#-------------------------------------------------------------------------------------------------------------
#add the newly created binutils to the path
export PATH=$INSTALLDIR/bin:$PATH
export AR_FOR_TARGET=arc-linux-uclibc-ar
export AS_FOR_TARGET=arc-linux-uclibc-as
export LD_FOR_TARGET=arc-linux-uclibc-ld
export NM_FOR_TARGET=arc-linux-uclibc-nm
export RANLIB_FOR_TARGET=arc-linux-uclibc-ranlib

#copy the bootstrap include files
cp -r bootstrap $INSTALLDIR/arc-linux-uclibc/include

#-------------------------------------------------------------------------------------------------------------
#copy the required Linux headers
if cp -r $LINUXDIR/include/linux $INSTALLDIR/arc-linux-uclibc/include ; then
 echo "#Linux headers found!" >> uclibc_log.txt
else
   echo [build_uclibc.sh] Fatal Error: \"$LINUXDIR\include/linux\" not found. Possibly incorrect or non-existent LINUXDIR specified to build_uclibc.sh.
   echo Usage: build_uclibc.sh INSTALLDIR LINUXDIR
   exit 1
fi

if cp -r $LINUXDIR/include/asm-arc $INSTALLDIR/arc-linux-uclibc/include/asm ; then
 echo "#Linux asm headers found!" >> uclibc_log.txt
else 
echo [build_uclibc.sh] Fatal Error: \"$LINUXDIR\include/asm-arc\" not found. Possibly incorrect or non-existent LINUXDIR specified to build_uclibc.sh.
   echo Usage: build_uclibc.sh INSTALLDIR LINUXDIR
   exit 1
fi
#-------------------------------------------------------------------------------------------------------------
echo "#build gcc" >> uclibc_log.txt
cd gcc/build
rm -r -f *
echo "Start building GCC!"
echo "..."
../src/configure --target=arc-linux-uclibc --prefix=$INSTALLDIR --with-headers=$INSTALLDIR/arc-linux-uclibc/include --enable-shared --disable-multilib --without-newlib --enable-languages=c,c++ --with-cpu=arc700 --disable-c99 >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt

if make all-gcc >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt; then
echo "Finish building GCC!"
else
 echo [build_uclibc.sh] Fatal Error: GCC build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ../..
 exit 1
fi
make install-gcc >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
cd ../..

#-------------------------------------------------------------------------------------------------------------
echo "#configure and build uClibc" >> uclibc_log.txt
echo "Start building uClibc!"
echo "..."
cd uClibc-0.9.29
 # Preparing a known working uclibc configuration.
 sed -e "s#%LINUX%#$LINUXDIR#" -e "s#%INSTALL%#$INSTALLDIR#"< arc_config > .config

if make CROSS=arc-linux-uclibc- >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt 3>> ../../uclibc_log.txt 4>> ../../uclibc_log.txt 5>> ../../uclibc_log.txt 6>> uclibc_log.txt; then
echo "Finish building uClibc!"
else
 echo [build_uclibc.sh] Fatal Error: uClibc build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ..
 exit 1
fi

make install >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt 

#copy the dynamic linker and make soft links
cp lib/ld-uClibc-0.9.29.so $INSTALLDIR/arc-linux-uclibc/lib
pushd . > /dev/zero 2> /dev/zero
cd $INSTALLDIR/arc-linux-uclibc/lib
ln -s ld-uClibc-0.9.29.so ld-uClibc.so
ln -s ld-uClibc-0.9.29.so ld-uClibc.so.0
popd > /dev/zero 2> /dev/zero
cd ..

#-------------------------------------------------------------------------------------------------------------
echo "C/C++ libs" >> uclibc_log.txt
cd gcc/build
echo "Start building C++ libs!"
echo "..."
if make >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt; then
 echo "Finish building C++ libs!"
else
 echo [build_uclibc.sh] Fatal Error: C/C++ library build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ../..
 exit 1
fi

make install >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt 
cd ../..

# Copy C++ libs
cp $INSTALLDIR/lib/libstdc++.so.6.0.9 $INSTALLDIR/arc-linux-uclibc/lib
cp $INSTALLDIR/lib/libstdc++.a $INSTALLDIR/arc-linux-uclibc/lib
mv $INSTALLDIR/include/c++ $INSTALLDIR/arc-linux-uclibc/include/c++

# Sync. sys-include and include dirs
rm -rf $INSTALLDIR/arc-linux-uclibc/sys-include > /dev/zero 2> /dev/zero
cp -rf $INSTALLDIR/arc-linux-uclibc/include $INSTALLDIR/arc-linux-uclibc/sys-include > /dev/zero 2> /dev/zero

# Make necessary soft links
pushd . > /dev/zero 2> /dev/zero
cd $INSTALLDIR/arc-linux-uclibc/lib
ln -s libstdc++.so.6.0.9 libstdc++.so.6
ln -s libstdc++.so.6.0.9 libstdc++.so
ln -s libm.so libm.so.0
ln -s libdl.so libdl.so.0
ln -s libgcc_s.so libgcc_s.so.0
popd > /dev/zero 2> /dev/zero
#-------------------------------------------------------------------------------------------------------------

echo "# build insight/gdb" >> uclibc_log.txt
echo "Start building insight/GDB!"
echo "..."
cd insight/build
rm -r -f *
../src/configure --target=arc-linux-uclibc --prefix=$INSTALLDIR --disable-werror >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt

if make >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt; then
 echo "Finish building insight/GDB!"
else
 echo [build_uclibc.sh] Fatal Error: insight/gdb build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ../..
 exit 1
fi

make install >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
cd ../..

#-------------------------------------------------------------------------------------------------------------
echo "Start building gdbserver!"
echo "..."
cd insight/src/gdb/gdbserver
./build_gdbserver.sh >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
if cp gdbserver $INSTALLDIR/bin; then
echo "Finish building gdbserver!"
else
 echo [build_uclibc.sh] Fatal Error: gdbserver build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ../../../..
 exit 1
fi
cd ../../../..

#-------------------------------------------------------------------------------------------------------------
