#!/bin/bash

# *** Modified from dist version to build in build-uclibc not just build

#	SCRIPT TO BUILD ARC-LINUX-UCLIBC TOOLKIT
#	----------------------------------------

# Invocation Syntax
#	build_uclibc.sh	<INSTALLDIR> <LINUXDIR>

# INSTALLDIR = Directory where arc-linux-uclibc- toolkit will be installed.
# LINUXDIR = Directory where ARC Linux sources have been placed.

INSTALLDIR=$1
LINUXDIR=$2

# Building the simulator will fail if the user has their SHELL as csh, et al.
# By forcing this in the environment, sim/common/genmloop.sh invoking ${SHELL}
# will always invoke /bin/sh properly.
SHELL=/bin/sh
export SHELL

function kernel_ver()
{
    K_FILENM="${LINUXDIR}/Makefile"
    if [ ! -e ${K_FILENM} ] ; then
        echo "Kernel Makefile Missing"
        exit 2
    fi

    X=`grep SUBLEVEL ${K_FILENM} | head -1 | awk '{print $3}'`
    echo $X
}

if [ "$INSTALLDIR" = "" ]; then
 echo Usage: build_uclibc.sh  INSTALLDIR  LINUXDIR
fi
if [ "$LINUXDIR" = "" ]; then
 echo Usage: build_uclibc.sh  INSTALLDIR  LINUXDIR
fi

echo "#build binutils" > uclibc_log.txt
echo "#-------------------------------------------------------------------" >> uclibc_log.txt
rm -rf binutils/build-uclibc && mkdir binutils/build-uclibc
cd binutils/build-uclibc # && rm -r -f *
echo "Start building BINUTILS!"
echo "..."
../src/configure --prefix=$INSTALLDIR --target=arc-linux-uclibc --disable-werror > ../../uclibc_log.txt 2>> ../../uclibc_log.txt

if make -j4 >> ../../uclibc_log.txt 2>&1 ; then
 echo "Finish building BINUTILS!"
else
 echo [build_uclibc.sh] Fatal Error: BINUTILS build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ../..
 exit 1
fi
make install >> ../../uclibc_log.txt 2>&1
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

# Get rid of permission-controlled SVN subdirs which can break the header copy
find $INSTALLDIR/arc-linux-uclibc/include -type d -name .svn | xargs rm -rf

#copy the required Linux headers
if cp -r $LINUXDIR/include/linux $INSTALLDIR/arc-linux-uclibc/include ; then
 echo "#Linux headers found!" >> uclibc_log.txt
else
   echo [build_uclibc.sh] Fatal Error: \"$LINUXDIR\include/linux\" not found. Possibly incorrect or non-existent LINUXDIR specified to build_uclibc.sh.
   echo Usage: build_uclibc.sh INSTALLDIR LINUXDIR
   exit 1
fi

cp -r $LINUXDIR/include/asm-generic $INSTALLDIR/arc-linux-uclibc/include/asm-generic

KVER=$( kernel_ver )
if [ `expr $KVER` -gt `expr 26` ]; then
    echo "Linux 2.6.30" >> uclibc_log.txt
    cp -r $LINUXDIR/arch/arc/include/asm $INSTALLDIR/arc-linux-uclibc/include
else
    echo "Linux 2.6.26 or lower" >> uclibc_log.txt
    cp -r $LINUXDIR/include/asm-arc $INSTALLDIR/arc-linux-uclibc/include/asm
fi

echo "#-------------------------------------------------------------------" >> uclibc_log.txt
echo "#build gcc" >> uclibc_log.txt
rm -rf gcc/build-uclibc && mkdir gcc/build-uclibc
cd gcc/build-uclibc # && rm -r -f *
echo "Start building GCC!"
echo "..."
../src/configure --target=arc-linux-uclibc --prefix=$INSTALLDIR --with-headers=$INSTALLDIR/arc-linux-uclibc/include --enable-shared --disable-multilib --without-newlib --enable-languages=c,c++ --with-cpu=arc700 --disable-c99 >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt

if make -j4 all-gcc >> ../../uclibc_log.txt 2>&1; then
echo "Finish building GCC!"
else
 echo [build_uclibc.sh] Fatal Error: GCC build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ../..
 exit 1
fi
make install-gcc >> ../../uclibc_log.txt 2>&1
cd ../..

echo "#-------------------------------------------------------------------" >> uclibc_log.txt
echo "#configure and build uClibc" >> uclibc_log.txt
echo "Start building uClibc!"
echo "..."
cd uClibc-0.9.29
 # Preparing a known working uclibc configuration.
 sed -e "s#%LINUX%#$LINUXDIR#" -e "s#%INSTALL%#$INSTALLDIR#"< arc_config > .config

if make CROSS=arc-linux-uclibc- >> ../uclibc_log.txt 2>&1 3>> ../uclibc_log.txt 4>> ../uclibc_log.txt 5>> ../uclibc_log.txt 6>> ../uclibc_log.txt; then
echo "Finish building uClibc!"
else
 echo [build_uclibc.sh] Fatal Error: uClibc build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ..
 exit 1
fi

make install >> ../uclibc_log.txt 2>&1

#copy the dynamic linker and make soft links
cp lib/ld-uClibc-0.9.29.so $INSTALLDIR/arc-linux-uclibc/lib
pushd . > /dev/zero 2> /dev/zero
cd $INSTALLDIR/arc-linux-uclibc/lib
ln -s ld-uClibc-0.9.29.so ld-uClibc.so
ln -s ld-uClibc-0.9.29.so ld-uClibc.so.0
popd > /dev/zero 2> /dev/zero
cd ..

echo "#-------------------------------------------------------------------" >> uclibc_log.txt
echo "C/C++ libs" >> uclibc_log.txt
cd gcc/build-uclibc
echo "Start building C++ libs!"
echo "..."
if make -j4 >> ../../uclibc_log.txt 2>&1; then
 echo "Finish building C++ libs!"
else
 echo [build_uclibc.sh] Fatal Error: C/C++ library build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ../..
 exit 1
fi

make install >> ../../uclibc_log.txt 2>&1
cd ../..

# Copy C++ libs
cp $INSTALLDIR/lib/libstdc++.so.6.0.9 $INSTALLDIR/arc-linux-uclibc/lib
cp $INSTALLDIR/lib/libstdc++.a $INSTALLDIR/arc-linux-uclibc/lib
mv $INSTALLDIR/include/c++ $INSTALLDIR/arc-linux-uclibc/include/c++

# Sync. sys-include and include dirs
rm -rf $INSTALLDIR/arc-linux-uclibc/sys-include
ln -s $INSTALLDIR/arc-linux-uclibc/include $INSTALLDIR/arc-linux-uclibc/sys-include

# Make necessary soft links
pushd . > /dev/zero 2> /dev/zero
cd $INSTALLDIR/arc-linux-uclibc/lib
ln -s libstdc++.so.6.0.9 libstdc++.so.6
ln -s libstdc++.so.6.0.9 libstdc++.so
ln -s libm.so libm.so.0
ln -s libdl.so libdl.so.0
ln -s libgcc_s.so libgcc_s.so.0
popd > /dev/zero 2> /dev/zero
echo "#-------------------------------------------------------------------" >> uclibc_log.txt

echo "# build insight/gdb" >> uclibc_log.txt
echo "Start building insight/GDB!"
echo "..."
rm -rf insight/build-uclibc && mkdir insight/build-uclibc
cd insight/build-uclibc # && rm -r -f *
../src/configure --target=arc-linux-uclibc --prefix=$INSTALLDIR --disable-werror >> ../../uclibc_log.txt 2>> ../../uclibc_log.txt

if make -j4 >> ../../uclibc_log.txt 2>&1; then
 echo "Finish building insight/GDB!"
else
 echo [build_uclibc.sh] Fatal Error: insight/gdb build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ../..
 exit 1
fi

make install >> ../../uclibc_log.txt 2>&1
cd ../..

echo "#-------------------------------------------------------------------" >> uclibc_log.txt
echo "Start building gdbserver!"
echo "..."
cd insight/src/gdb/gdbserver
./build_gdbserver.sh >> ../../../../uclibc_log.txt 2>&1
if cp gdbserver $INSTALLDIR/bin; then
echo "Finish building gdbserver!"
else
 echo [build_uclibc.sh] Fatal Error: gdbserver build was not successful.
 echo Please see \"uclibc_log.txt\" for details.
 cd ../../../..
 exit 1
fi
cd ../../../..
echo "#-------------------------------------------------------------------" >> uclibc_log.txt
