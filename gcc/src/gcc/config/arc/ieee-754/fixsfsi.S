#include "arc-ieee-754.h"

#if 0 /* DEBUG */
	.global __fixsfsi
	FUNC(__fixsfsi)
	.balign 4
__fixsfsi:
	push_s blink
	bl.d __fixsfsi_c
	push_s r0
	ld_s r1,[sp]
	st_s r0,[sp]
	bl.d __fixsfsi_asm
	mov_s r0,r1
	pop_s r1
	pop_s blink
	cmp r0,r1
	jeq_s [blink]
	bl abort
	ENDFUNC(__fixsfsi)
#define __fixsfsi __fixsfsi_asm
#endif /* DEBUG */

	.global __fixsfsi
	FUNC(__fixsfsi)
	.balign 4
__fixsfsi:
	bbit0 r0,30,.Lret0or1
	lsr r2,r0,23
	bmsk_s r0,r0,22
	bset_s r0,r0,23
	sub_s r2,r2,22;0x7f+23-0x80
	asl.f 0,r2,24
	neg r3,r2
	asl.mi r0,r0,r2
	lsr.pl r0,r0,r3
	j_s.d [blink]
	neg.cs r0,r0
.Lret0or1:
	add.f r0,r0,0x800000
	lsr_s r0,r0,30

	bmsk_s r0,r0,0
	j_s.d [blink]
	neg.mi r0,r0
	ENDFUNC(__fixsfsi)
