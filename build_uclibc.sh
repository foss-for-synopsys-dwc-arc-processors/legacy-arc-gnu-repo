#!/bin/sh

#	SCRIPT TO BUILD ARC-LINUX-UCLIBC TOOLKIT
#	----------------------------------------

# Invocation Syntax
#	build_uclibc.sh	<INSTALLDIR> <LINUXDIR>

# INSTALLDIR = Directory where arc-linux-uclibc- toolkit will be installed.
# LINUXDIR = Directory where ARC Linux sources have been placed.

INSTALLDIR=$1
LINUXDIR=$2

echo "#build binutils" >> uclibc_log.txt
echo "#-------------------------------------------------------------------------------------------------------------" >> uclibc_log.txt
cd binutils/build
rm -r -f *
echo "Start building BINUTILS!"
echo "..."
../src/configure --prefix=$INSTALLDIR --target=arc-linux-uclibc --disable-werror > ../../uclibc_log.txt 2>> ../../uclibc_log.txt
make >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
echo "Finish building BINUTILS!"
echo "Press Enter to continue"
read
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

#copy the required Linux headers
cp -r $LINUXDIR/include/linux $INSTALLDIR/arc-linux-uclibc/include
cp -r $LINUXDIR/include/asm-arc $INSTALLDIR/arc-linux-uclibc/include/asm

echo "#build gcc" >> uclibc_log.txt
cd gcc/build
rm -r -f *
echo "Start building GCC!"
echo "..."
../src/configure --target=arc-linux-uclibc --prefix=$INSTALLDIR --with-headers=$INSTALLDIR/arc-linux-uclibc/include --enable-shared --disable-multilib --without-newlib --enable-languages=c,c++ --with-cpu=arc700 --disable-c99 >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
make all-gcc >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
echo "Finish building GCC!"
echo "Press Enter to continue"
read
make install-gcc >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
cd ../..

#-------------------------------------------------------------------------------------------------------------
echo "#configure and build uClibc" >> uclibc_log.txt
echo "Start building uClibc!"
echo "..."
cd uClibc-0.9.29
 # Preparing a known working uclibc configuration.
 sed -e "s#%LINUX%#$LINUXDIR#" -e "s#%INSTALL%#$INSTALLDIR#"< arc_config > .config
make CROSS=arc-linux-uclibc- >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
echo "Finish building uClibc!"
echo "Press Enter to continue"
read
make install >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
#copy the dynamic linker
cp lib/ld-uClibc.so* $INSTALLDIR/arc-linux-uclibc/lib
cd ..

#-------------------------------------------------------------------------------------------------------------
echo "C/C++ libs" >> uclibc_log.txt
cd gcc/build
echo "Start building C++ libs!"
echo "..."
make >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
echo "Finish building C++ libs!"
echo "Press Enter to continue"
read
make install >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
cd ../..

# Copy C++ libs
cp $INSTALLDIR/lib/libstdc++.so* $INSTALLDIR/arc-linux-uclibc/lib
cp $INSTALLDIR/lib/libstdc++.a $INSTALLDIR/arc-linux-uclibc/lib
cp -r $INSTALLDIR/include/c++ $INSTALLDIR/arc-linux-uclibc/include

# Make necessary soft links
pushd .
cd $INSTALLDIR/arc-linux-uclibc/lib
ln -s libm.so libm.so.0
ln -s libdl.so libdl.so.0
ln -s libgcc_s.so libgcc_s.so.0
popd
#-------------------------------------------------------------------------------------------------------------

echo "# build insight/gdb" >> uclibc_log.txt
echo "Start building insight/GDB!"
echo "..."
cd insight/build
rm -r -f *
../src/configure --target=arc-linux-uclibc --prefix=$INSTALLDIR --disable-werror >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
make >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
echo "Finish building insight/GDB!"
make install >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
cd ../..

#-------------------------------------------------------------------------------------------------------------
echo "Start building gdbserver!"
echo "..."
cd insight/src/gdb/gdbserver
./build_gdbserver.sh >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt
cp gdbserver $INSTALLDIR/bin
cd ../../../..
echo "Finish building gdbserver!"
#-------------------------------------------------------------------------------------------------------------
