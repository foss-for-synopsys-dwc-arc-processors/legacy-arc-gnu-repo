GDB/insight 6.6 for ARC700/ARC600
=================================

I) Notes
II) Building / Installing GDB
III) Building / Installing GDB Server


I) Notes
========

This is a merged version of FSF GDB/insight 6.6 and 6.3_codito_20060518 release.


II) Building /Installing GDB
============================

Building is best done is a separate directory to the source tree, this enables 
quick and clean re-configurations/rebuilds as your needs change.

Un-tar/zip the distribution.  Create a build directory, and change into that 
directory:

$ mkdir build
$ cd build

Binutils can be configured to support one of two target types - 'arc-elf32' and
'arc-linux-uclibc'.  The former is for debugging 'bare-metal' builds of user code
(i.e. built with newlib), the latter is for deugging user mode Linux applications, 
via communication to the remote gdbserver process, running on Linux for ARC700.

Configure the build, and specify where you want it to be installed (e.g. /opt/arc):

$ ../src/configure --target=arc-elf32 --prefix=/opt/arc --disable-werror
..or..
$ ../src/configure --target=arc-linux-uclibc --prefix=/opt/arc --disable-werror

Make and install

$ make
$ make install

This will install into the bin/include/lib/.. directories off of the prefix specified 
during configure (e.g. the gdb will be at /opt/arc/bin/arc-elf32-gdb).

Note: non-prefix'd versions of the executables will also be found in the arc-elf32 /
arc-linux-uclibc directories depending on your selected configuration.

     Troubleshooting:
     ================
     [1] Please make sure that your default SHELL is bash. You can try
      > echo $SHELL	
      It must be /bin/bash.

     [2] If you get errors of some kind of missing headers, your host system must be missing
     some required packages/libraries. Please make sure that you have installed following packages
     in addition of to basic ones

     texinfo
     flex
     bison
     byacc
     termcap
     qt (or qt-devel)
     libtermcap (or libtermcap-devel)
     ncurses (or libtermcap-devel)
         

II) Building /Installing GDB Server
===================================

GDB server is a user-mode application, run on the target machine, which is running Linux.
Therefore GDB server must be built with the correct target machine toolchain (in this case
arc-linux-uclibc).


$ cd src/gdb/gdb/gdbserver
$ ./configure --host=i386-redhat-linux-gnu --target=arc-linux-uclibc --prefix=/opt/arc

Note: The generated Makefile will require that 'gcc' is the target gcc, so either you need
to temporarily point your path to the installed ARC700 tools without the 'arc-linux-uclibc-'
prefix (not recommended), or edit the Makefile and change the line:

CC=gcc

to

CC=arc-linux-uclibc-gcc

Then perform a normal build and install


$ make
$ make install

Please use script at <SOURCE-DIR>/insight/src/build-gdbserver.sh for reference.

Note: the generated gdbserver binary is for ARC700 and NOT for x86, therefore you should copy
it into your ARC700 Linux distribution, ready for use.


