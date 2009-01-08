/* setjmp for ARC, ELF version.
   Copyright (C) 1995, 1996, 1997, 2000, 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#define __ASSEMBLY__
#define _ASM
#define _SETJMP_H
#include <bits/setjmp.h>
#include <sys/syscall.h>

.globl ___sigsetjmp;
.type   ___sigsetjmp,@function
.align 4;

___sigsetjmp:
	; save the regs
	st	r4,  [r0]
	st	r5,  [r0, 4]
	st	r6,  [r0, 8]
	st	r7,  [r0, 12]
	st	r8,  [r0, 16]
	st	r9,  [r0, 20]

	st	fp,  [r0, 24]	;  the fp
	st	sp,  [r0, 28]	;  the sp

	st	blink, [r0, 32]	;  the blink, ie the pc

	b.nd	ARC_SYMBOL_NAME(__sigjmp_save)

.size ___sigsetjmp,.-___sigsetjmp

.globl ARC_SYMBOL_NAME(__sigsetjmp)
	ARC_SYMBOL_NAME(__sigsetjmp) = ___sigsetjmp
