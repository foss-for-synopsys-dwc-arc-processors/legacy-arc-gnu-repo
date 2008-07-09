#include "arc-ieee-754.h"

#if 0 /* DEBUG */
	FUNC(__fixdfsi)
	.global __fixdfsi
	.balign 4
__fixdfsi:
	push_s blink
	push_s r0
	bl.d __fixdfsi_c
	push_s r1
	mov_s r2,r0
	pop_s r1
	ld r0,[sp]
	bl.d __fixdfsi_asm
	st r2,[sp]
	pop_s r1
	pop_s blink
	cmp r0,r1
	jeq_s [blink]
	bl abort
	ENDFUNC(__fixdfsi)
#define __fixdfsi __fixdfsi_asm
#endif /* DEBUG */

	.global __fixdfsi
	FUNC(__fixdfsi)
	.balign 4
__fixdfsi:
	bbit0 DBL0H,30,.Lret0or1
	lsr r2,DBL0H,20
	bmsk_s DBL0H,DBL0H,19
	bset_s DBL0H,DBL0H,20
	sub_s r2,r2,19; 0x3ff+20-0x400
	asl.f 0,r2,21
	neg r3,r2
#ifdef __LITTLE_ENDIAN__
	mov.pl DBL0L,DBL0H
	asl DBL0H,DBL0H,r2
#else
	asl.mi DBL0H,DBL0H,r2
	lsr.pl DBL0H,DBL0H,r3
#endif
	lsr DBL0L,DBL0L,r3

	add.mi r0,r0,r1
	j_s.d [blink]
	neg.cs r0,r0
.Lret0or1:
	add.f r0,DBL0H,0x100000
	lsr_s r0,r0,30

	bmsk_s r0,r0,0
	j_s.d [blink]
	neg.mi r0,r0
	ENDFUNC(__fixdfsi)
