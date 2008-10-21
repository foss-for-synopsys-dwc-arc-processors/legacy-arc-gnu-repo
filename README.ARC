GCC toolchain for ARC700/ARC600 20080121
=============================================


I) Overview
II) Building 


I) Overview
===========

This source package contains ported versions of the following tools to the
ARC series of processors:

i)   Binutils 2.17, including assembler, linker and other utilities
ii)  GCC 4.2, including C/C++ compilers and libraries (newlib)
iii) GDB 6.8 source level debugger with hardware (JTAG) debug support, CGEN-based simulator and insight (GDB front-end).
iv)  uClibc 0.9.29, user-mode runtime library

Relevant pdf manuals are also available at <untar location>/docs.


II) Building
============

Each sub-directory contains a README.ARC file detailing how to build/install the
specific toolset.

For 'arc-elf32' targets, the build order should be:

1) binutils
2) gcc
3) insight

For 'arc-linux-uclibc' targets the build order should be:

1) binutils and gcc for 'arc-elf32' (required to build uClibc)
2) uClibc
3) binutils
4) gcc
5) insight
6) gdbserver (within gdb source tree)

Moreover quick toolkit building scripts named build_elf32.sh and build_uclibc.sh are also provided. However due to difference in Linux enviornments (gcc/make/bison versions) you may need a bit of tweaking.

