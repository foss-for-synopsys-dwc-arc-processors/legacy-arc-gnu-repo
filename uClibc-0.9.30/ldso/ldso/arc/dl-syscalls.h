/******************************************************************************
 * Copyright Synopsys, Inc. (www.synopsys.com) Oct 01, 2004
 * 
 *
 * Licensed under the LGPL v2.1, see the file COPYING.LIB in this tarball.
 *
 *****************************************************************************/

/* Define the __set_errno macro as nothing so that INLINE_SYSCALL
 * won't set errno, which is important since we make system calls
 * before the errno symbol is dynamicly linked. */

#define __set_errno(X) {(void)(X);}
#define MMAP2_PAGE_SHIFT PAGE_SHIFT

#include "sys/syscall.h"

