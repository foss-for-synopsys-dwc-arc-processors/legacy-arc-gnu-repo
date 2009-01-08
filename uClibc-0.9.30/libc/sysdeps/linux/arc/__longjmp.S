/* longjmp for ARC.
   Copyright (C) 1997, 1998 Free Software Foundation, Inc.
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

#define _SETJMP_H
#define _ASM
#include <bits/setjmp.h>

.globl ___longjmp;
.globl __longjmp;
.type ___longjmp, @function
.align 4;

___longjmp:
__longjmp:
	mov	r26, r0		; save jmp_buf pointer
	
	mov.f	r0, r1		; get the return value in place
	nop
 	add.z	r0, r0, 1	; cant let setjmp return zero

	; stupid, stupid, stupid assembler... ' not allowed in comments! (can't above)

	; load the regs
	ld	r4,  [r26]
	ld.a	r5,  [r26, 4]
	ld.a	r6,  [r26, 4]
	ld.a	r7,  [r26, 4]
	ld.a	r8,  [r26, 4]
	ld.a	r9,  [r26, 4]

	ld.a	fp, [r26, 4]	; the fp
	ld.a	sp, [r26, 4]	; the sp

	ld.a	blink, [r26, 4]
	add	r26, r26, 4	; increment r26 to make it point to the saved blink
	
	j.nd	[blink]
.size ___longjmp,.-___longjmp;
libc_hidden_def(__longjmp)
