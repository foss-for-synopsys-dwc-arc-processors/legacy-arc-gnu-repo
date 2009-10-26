#!/bin/sh

#	SCRIPT TO BUILD ARC-ELF32 TOOLKIT
#	---------------------------------

# Invocation Syntax
#	build_elf32.sh	<INSTALLDIR>

# INSTALLDIR = Directory where arc-elf32- toolkit will be installed.


INSTALLDIR=$1

#build binutils
mkdir -p binutils/build gcc/build insight/build
cd binutils/build
../src/configure --prefix=$INSTALLDIR --target=arc-elf32 --disable-werror  > ../../log.txt 2>> ../../log.txt
make  >> ../../log.txt 2>> ../../log.txt
make install  >> ../../log.txt 2>> ../../log.txt
cd ../..

#add the newly created binutils to the path
export PATH=$INSTALLDIR/bin:$PATH
export AR_FOR_TARGET=arc-elf32-ar
export AS_FOR_TARGET=arc-elf32-as
export LD_FOR_TARGET=arc-elf32-ld
export NM_FOR_TARGET=arc-elf32-nm
export RANLIB_FOR_TARGET=arc-elf32-ranlib

#build gcc
cd gcc/build
../src/configure --target=arc-elf32 --prefix=$INSTALLDIR --with-headers --enable-multilib --with-newlib --enable-languages=c,c++ --disable-shared  >> ../../log.txt 2>> ../../log.txt
make  >> ../../log.txt 2>> ../../log.txt
make install  >> ../../log.txt 2>> ../../log.txt
cd ../..

# Copy include/libs
cp -r $INSTALLDIR/include/c++ $INSTALLDIR/arc-elf32/include/
cp $INSTALLDIR/lib/libs*.a $INSTALLDIR/arc-elf32/lib/
cp $INSTALLDIR/lib/arc700/libs*.a $INSTALLDIR/arc-elf32/lib/arc700/

#build insight
cd insight/build
../src/configure --target=arc-elf32 --prefix=$INSTALLDIR --disable-werror  >> ../../log.txt 2>> ../../log.txt
make  >> ../../log.txt 2>> ../../log.txt
make install  >> ../../log.txt 2>> ../../log.txt
cd ../..



