/******************************************************************************
 * Copyright Codito Technologies (www.codito.com) Oct 01, 2004
 * 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 *****************************************************************************/

/*
 * This function is _not_ called directly.  It is jumped to (so no return
 * address is on the stack) when attempting to use a symbol that has not yet
 * been resolved.  The first time a jump symbol (such as a function call inside
 * a shared library) is used (before it gets resolved) it will jump here to
 * _dl_linux_resolve.  When we get jumped to all that we have is the pc address
 * of the PLT Entry that was used. That should be used to identify the relocation
 * entry to be used. The module info(tpnt) from (GOT + 4) which would identify 
 * the module from which the call came is in r11. 
 *	reloc_entry
 *	tpnt
 *
 * This function saves all the registers, puts a copy of reloc_entry and tpnt
 * on the stack (as function arguments) then make the function call
 * _dl_linux_resolver(tpnt, reloc_entry).  _dl_linux_resolver() figures out
 * where the jump symbol is _really_ supposed to have jumped to and returns
 * that to us.  Once we have that, we overwrite tpnt with this fixed up
 * address. We then clean up after ourselves, put all the registers back how we
 * found them, then we jump to where the fixed up address, which is where the
 * jump symbol that got us here really wanted to jump to in the first place.
 * found them, then we jump to the fixed up address, which is where the jump
 * symbol that got us here really wanted to jump to in the first place.  
 *  -Erik Andersen
 */
/* Save caller saved registers (scratch registers) ( r0 - r15, r24, r25 )
 * Registers are pushed / popped in the order defined in struct ptregs
 * in asm/ptrace.h
 */
 #include <sys/syscall.h>
.macro	SAVE_CALLER_SAVED
	st.a	r0, [sp, -4]
	st.a	r1, [sp, -4]
	st.a	r2, [sp, -4]
	st.a	r3, [sp, -4]
	st.a	r4, [sp, -4]
	st.a	r5, [sp, -4]
	st.a	r6, [sp, -4]
	st.a	r7, [sp, -4]
	st.a	r8, [sp, -4]
	st.a	r9, [sp, -4]
	st.a	r10, [sp, -4]
	st.a	r11, [sp, -4]
	st.a	r12, [sp, -4]
	st.a	r13, [sp, -4]
	st.a	r14, [sp, -4]
	st.a	r15, [sp, -4]
	st.a	r24, [sp, -4]
	st.a	r25, [sp, -4]
	st.a    blink,[sp, -4]
.endm

/* Restore caller saved registers (scratch registers)
 * Registers are pushed / popped in the order defined in struct callee_regs
 * in asm/ptrace.h
 */
.macro RESTORE_CALLER_SAVED
	ld.ab   blink,[sp,4]
	ld.ab	r25, [sp, 4]	
	ld.ab	r24, [sp, 4]	
	ld.ab	r15, [sp, 4]	
	ld.ab	r14, [sp, 4]	
	ld.ab	r13, [sp, 4]	
	ld.ab	r12, [sp, 4]	
	ld.ab	r11, [sp, 4]	
	ld.ab	r10, [sp, 4]	
	ld.ab	r9, [sp, 4]	
	ld.ab	r8, [sp, 4]	
	ld.ab	r7, [sp, 4]	
	ld.ab	r6, [sp, 4]	
	ld.ab	r5, [sp, 4]	
	ld.ab	r4, [sp, 4]	
	ld.ab	r3, [sp, 4]	
	ld.ab	r2, [sp, 4]	
	ld.ab	r1, [sp, 4]	
	ld.ab	r0, [sp, 4]
.endm	

.text
.align 4

.globl __dl_linux_resolve
.globl _dl_linux_resolve
.type	_dl_linux_resolve,@function
.type  __dl_linux_resolve,@function

__dl_linux_resolve:		
_dl_linux_resolve:
		/* preserve all regs */
		/* eax = tpnt and reloc_entry params */
		/* push copy of reloc_entry param */
		/* push copy of tpnt param */
		sub sp, sp,4
	        SAVE_CALLER_SAVED 
	        mov r1,r12
		mov r0,r11
		bl ARC_SYMBOL_NAME(_dl_linux_resolver)
		st r0,[sp,76]
		RESTORE_CALLER_SAVED 
		ld r12,[sp]
		j.d  [r12]
		add  sp,sp,4
		/* store func addr over original
					 * tpnt param */
		/* remove copy parameters */
			/* restore regs */
		/* jump to func removing original
					 * reloc_entry param from stack */
.LFE2:
	.size _dl_linux_resolve,.LFE2-_dl_linux_resolve


 
	
