GCC for ARC700/ARC600 20071024
==============================

I) Prerequisites
II) Building / Installing
III) Running


I) Prerequisites
================

In order to build a useful system, you should at least have already built and
installed binutils for your target system.  If you are going to build the
uClibc version of GCC, then you should also have a built/installed version
of the uClibc library (which itself is compiled with the arc-elf32 version of
GCC).  The arc-elf32 target build will include the newlib and stdc++ libraries
allowing 'bare-metal' builds.

II) Building /Installing
=======================

Building is best done is a separate directory to the source tree, this enables 
quick and clean re-configurations/rebuilds as your needs change.

Un-tar/zip the distribution.  Create a build directory, and change into that 
directory:

$ mkdir build
$ cd build


GCC can be configured to support one of two target types - 'arc-elf32' and
'arc-linux-uclibc'.  The former is for 'bare-metal' builds, the latter is for
building user mode Linux applications, to be linked against the uClibc library.

You should ensure that the pre-built/installed version of binutils is available in 
the path, it is also advisable to setup the following environment variables:

For target arc-elf32:

$ export PATH=<PATH TO ARC-ELF32 BINUTILS BINARIES>:$PATH
$ export AR_FOR_TARGET=arc-elf32-ar
$ export AS_FOR_TARGET=arc-elf32-as
$ export LD_FOR_TARGET=arc-elf32-ld
$ export NM_FOR_TARGET=arc-elf32-nm
$ export RANLIB_FOR_TARGET=arc-elf32-ranlib

For target arc-linux-uclibc:

$ export PATH=<PATH TO ARC-LINUX-UCLIBC BINUTILS BINARIES>:$PATH
$ export AR_FOR_TARGET=arc-linux-uclibc-ar
$ export AS_FOR_TARGET=arc-linux-uclibc-as
$ export LD_FOR_TARGET=arc-linux-uclibc-ld
$ export NM_FOR_TARGET=arc-linux-uclibc-nm
$ export RANLIB_FOR_TARGET=arc-linux-uclibc-ranlib

Configure the build, and specify where you want it to be installed (e.g. /opt/arc):

$ ../src/configure  --prefix=/opt/arc --target=arc-elf32 --with-newlib --with-headers --enable-multilib --enable-languages=c,c++

..or..

$ ../src/configure --prefix=/opt/arc/uclibc --target=arc-linux-uclibc --without-newlib --disable-multilib --with-cpu=arc700 \
 --disable-c99 --enable-languages=c,c++

Make and install

$ make
$ make install

This will install into the bin/include/lib/.. directories off of the prefix specified 
during configure (e.g. the compiler will be at /opt/arc/bin/arc-elf32-gcc).

Note: non-prefix'd versions of the executables will also be found in the arc-elf32 /
arc-linux-uclibc directories depending on your selected configuration.

II) Running
===========

The built GCC toolchain will support ARC target processors, but otherwise remain the same as
other GCC compiler releases.

