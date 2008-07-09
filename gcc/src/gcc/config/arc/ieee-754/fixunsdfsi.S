#include "arc-ieee-754.h"

#if 0 /* DEBUG */
	FUNC(__fixunsdfsi)
	.global __fixunsdfsi
	.balign 4
__fixunsdfsi:
	push_s blink
	push_s r0
	bl.d __fixunsdfsi_c
	push_s r1
	mov_s r2,r0
	pop_s r1
	ld r0,[sp]
	bl.d __fixunsdfsi_asm
	st r2,[sp]
	pop_s r1
	pop_s blink
	cmp r0,r1
	jeq_s [blink]
	bl abort
	ENDFUNC(__fixunsdfsi)
#define __fixunsdfsi __fixunsdfsi_asm
#endif /* DEBUG */

	.global __fixunsdfsi
	FUNC(__fixunsdfsi)
	.balign 4
__fixunsdfsi:
	bbit0 DBL0H,30,.Lret0or1
	lsr r2,DBL0H,20
	bmsk_s DBL0H,DBL0H,19
	sub_s r2,r2,19; 0x3ff+20-0x400
	neg_s r3,r2
	btst_s r3,10
	bset_s DBL0H,DBL0H,20
#ifdef __LITTLE_ENDIAN__
	mov.ne DBL0L,DBL0H
	asl DBL0H,DBL0H,r2
#else
	asl.eq DBL0H,DBL0H,r2
	lsr.ne DBL0H,DBL0H,r3
#endif
	lsr DBL0L,DBL0L,r3
	j_s.d [blink]
	add.eq r0,r0,r1
.Lret0:
	j_s.d [blink]
	mov_l r0,0
.Lret0or1:
	add_s DBL0H,DBL0H,0x100000
	lsr_s DBL0H,DBL0H,30
	j_s.d [blink]
	bmsk_l r0,DBL0H,0
	ENDFUNC(__fixunsdfsi)
