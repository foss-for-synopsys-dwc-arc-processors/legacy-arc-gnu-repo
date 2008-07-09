#include "arc-ieee-754.h"

#if 0 /* DEBUG */
	.global __floatsidf
	.balign 4
	FUNC(__floatsidf)
__floatsidf:
	push_s blink
	bl.d __floatsidf_c
	push_s r0
	ld_s r2,[sp]
	st_s r1,[sp]
	push_s r0
	bl.d __floatsidf_asm
	mov_s r0,r2
	pop_s r2
	pop_s r3
	pop_s blink
	cmp r0,r2
	cmp.eq r1,r3
	jeq_s [blink]
	bl abort
	ENDFUNC(__floatsidf)
#define __floatsidf __floatsidf_asm
#endif /* DEBUG */

	.global __floatsidf
	.balign 4
	FUNC(__floatsidf)
__floatsidf:
	abs.f r1,r0
	jeq_s [blink]
	lsr r2,r1
	mov r12,-0x41d ; -(0x3ff+31-1)
	norm r2,r2
	bclr.cs r12,r12,11
	rsub.f r3,r2,11
	add_s r12,r2,r12
	add_s r2,r2,21
#ifdef __LITTLE_ENDIAN__
	asl DBL0L,r1,r2
	lsr_s DBL0H,r1,r3
#else
	lsr DBL0H,r1,r3
	asl_s DBL0L,r1,r2
#endif
	asl_s r12,r12,20
	mov.lo DBL0H,DBL0L
	sub_s DBL0H,DBL0H,r12
	j_s.d [blink]
	mov.ls DBL0L,0
	ENDFUNC(__floatsidf)
