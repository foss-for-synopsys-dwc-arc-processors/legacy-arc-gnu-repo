# This is a script to build the toolchain on LSF, e.g. on saturn do:
# ./build-elf32-uclibc.sh ~/install-linux-uclibc ~/linux26
export INSTALLDIR=$1
export LINUXDIR=$2
export TRUNKDIR=`pwd`
export PATH=$INSTALLDIR/elf32/bin:$INSTALLDIR/uclibc/bin:/home/joernr/rtest/bin:$PATH
export ELF32_TOOLNAMES='\
  export AR_FOR_TARGET=arc-elf32-ar;\
  export AS_FOR_TARGET=arc-elf32-as;\
  export LD_FOR_TARGET=arc-elf32-ld;\
  export NM_FOR_TARGET=arc-elf32-nm;\
  export RANLIB_FOR_TARGET=arc-elf32-ranlib'
export UCLIBC_TOOLNAMES='\
  export AR_FOR_TARGET=arc-uclibc-ar;\
  export AS_FOR_TARGET=arc-uclibc-as;\
  export LD_FOR_TARGET=arc-uclibc-ld;\
  export NM_FOR_TARGET=arc-uclibc-nm;\
  export RANLIB_FOR_TARGET=arc-uclibc-ranlib'
export MAKE='make -j -l 8'
make -j 8
