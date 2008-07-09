Binutils for ARC700/ARC600 20071024
===================================

I) Building / Installing
II) Running


I) Building /Installing
=======================

Building is best done is a separate directory to the source tree, this enables 
quick and clean re-configurations/rebuilds as your needs change.

Un-tar/zip the distribution.  Create a build directory, and change into that 
directory:

$ mkdir build
$ cd build


Binutils can be configured to support one of two target types - 'arc-elf32' and
'arc-linux-uclibc'.  The former is for 'bare-metal' builds, the latter is for
building user mode Linux applications, to be linked against the uClibc library.

Configure the build, and specify where you want it to be installed (e.g. /opt/arc):

$ ../src/configure --target=arc-elf32 --prefix=/opt/arc --disable-werror
..or..
$ ../src/configure --target=arc-linux-uclibc --prefix=/opt/arc --disable-werror

Make and install

$ make
$ make install

This will install into the bin/include/lib/.. directories off of the prefix specified 
during configure (e.g. the assembler will be at /opt/arc/bin/arc-elf32-as).

Note: non-prefix'd versions of the executables will also be found in the arc-elf32 /
arc-linux-uclibc directories depending on your selected configuration.

II) Running
===========

The binary utilities support ARC target processors, but otherwise remain the same as
other binutils releases.

