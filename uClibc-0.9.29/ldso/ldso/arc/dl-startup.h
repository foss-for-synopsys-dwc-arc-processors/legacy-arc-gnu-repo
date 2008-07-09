/*
 * Architecture specific code used by dl-startup.c
 *
 * Copyright Codito Technologies (www.codito.com) Oct 01, 2004
 * Copyright (C) 2005 by Erik Andersen <andersen@codepoet.org>
 * Copyright (C) 2007 ARC International (UK) LTD
 */

#include <sys/syscall.h>

#ifdef GAS_FIXED
#define LEAPCREL(DST,SRC) "	add "#DST",pcl,"#SRC"-(.&-4)\n"
#define LDPCREL(DST,SRC) "	ld "#DST",[pcl,"#SRC"-(.&-4)]\n"
#else
#define LEAPCREL(DST,SRC) "\
        st.a gp,[sp,-4]\n\
        add gp, pcl, @_DYNAMIC@gotpc\n\
        add "#DST",gp,@"#SRC"@gotoff\n\
        ld.ab gp,[sp,4]\n"
#define LDPCREL(DST,SRC) LEAPCREL (DST,SRC) "\
	ld "#DST",["#DST"]\n"
#endif

asm("\
	.text\n\
	.balign	4\n\
	.globl	_start\n\
	.type _start,@function\n\
_start:\n\
	mov_s	r0, sp\n\
	bl	_dl_start\n\
	mov_s	r12,r0\n\
	; See if we were run as a command with the executable file\n\
	; name as an extra leading argument.\n"
	LDPCREL	(r0,_dl_skip_args)
"	; get the original argument count\n\
	ld_s	r1,[sp]\n\
	; Adjust the stack pointer to skip _dl_skip_args words.\n\
	add2	sp,sp,r0\n\
	; Subtract _dl_skip_args from original argument count.\n\
	sub_s	r1,r1,r0\n\
	; Store back the modified argument count.\n\
	st_s	r1,[sp]\n\
	;  Pass our finalizer function to the user in %r0.\n"
	LEAPCREL (r0, _dl_fini)
"	j_s	[r12]\n\
	.size	_start,.-_start\n\
	.previous\n");


/* Function call should be fine as long as the function is resolved locally to
   the DSO and we're not using the large memory model, since the default
   way to do function calls is the bl instruction, which uses pc-relative
   addressing.  */
#if 0
/* We can't call functions earlier in the dl startup process */
/* TODO: check if that is true.  */
#define NO_FUNCS_BEFORE_BOOTSTRAP
#endif
