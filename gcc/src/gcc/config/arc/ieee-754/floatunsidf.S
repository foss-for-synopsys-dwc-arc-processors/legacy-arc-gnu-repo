#include "arc-ieee-754.h"

#if 0 /* DEBUG */
	.global __floatunsidf
	.balign 4
	FUNC(__floatunsidf)
__floatunsidf:
	push_s blink
	bl.d __floatunsidf_c
	push_s r0
	ld_s r2,[sp]
	st_s r1,[sp]
	push_s r0
	bl.d __floatunsidf_asm
	mov_s r0,r2
	pop_s r2
	pop_s r3
	pop_s blink
	cmp r0,r2
	cmp.eq r1,r3
	jeq_s [blink]
	bl abort
	ENDFUNC(__floatunsidf)
#define __floatunsidf __floatunsidf_asm
#endif /* DEBUG */

	.global __floatunsidf
	.balign 4
	FUNC(__floatunsidf)
__floatunsidf:
	lsr_s r1,r0
	breq_s r0,0,.Lret0
	norm r2,r1
	mov r12,-0x41d ; -(0x3ff+31-1)
	rsub.f r3,r2,11
	add_s r12,r2,r12
	add_s r2,r2,21
#ifdef __LITTLE_ENDIAN__
	lsr DBL0H,r0,r3
	asl_s DBL0L,r0,r2
#else
	asl DBL0L,r0,r2
	lsr_s DBL0H,r0,r3
#endif
	asl_s r12,r12,20
	mov.lo DBL0H,DBL0L
	sub_s DBL0H,DBL0H,r12
.Lret0:	j_s.d [blink]
	mov.ls DBL0L,0
	ENDFUNC(__floatunsidf)
