#include "arc-ieee-754.h"
/* inputs: DBL0, DBL1
   output: z flag
   clobber: r12, flags
   For NaNs, bit 19.. bit 30 of the high word must be set.  */
#if 0 /* DEBUG */
	.global __eqdf2
	.balign 4
	FUNC(__eqdf2)
__eqdf2:
	st.a r11,[sp,-4]` push_s blink` st.a r10,[sp,-4]` st.a r9,[sp,-4]
	st.a r8,[sp,-4]` st.a r7,[sp,-4]` st.a r6,[sp,-4]` st.a r5,[sp,-4]
	st.a r4,[sp,-4]` push_s r3` push_s r2` push_s r1`
	bl.d __eqdf2_c` push_s r0
	mov r11,r0` pop_s r0` pop_s r1` pop_s r2` pop_s r3
	ld.ab r4,[sp,4]` ld.ab r5,[sp,4]` ld.ab r6,[sp,4]`
	ld.ab r7,[sp,4]` ld.ab r8,[sp,4]` ld.ab r9,[sp,4]
	bl.d __eqdf2_asm` ld.ab r10,[sp,4]
	pop_s blink
	breq.d r11,0,0f
	ld.ab r11,[sp,4]
	jne_s [blink]
	bl abort
0:	jeq_s [blink]
	bl abort
	ENDFUNC(__eqdf2)
#define __eqdf2 __eqdf2_asm
#endif /* DEBUG */
	.global __eqdf2
	.balign 4
	FUNC(__eqdf2)
	/* Good performance as long as the difference in high word is
	   well predictable (as seen from the branch predictor).  */
__eqdf2:
	brne.d DBL0H,DBL1H,.Lhighdiff
	bmsk r12,DBL0H,20
	add1.f	r12,r12,DBL0H /* set c iff NaN; also, clear z if NaN.  */
	j_s.d	[blink]
	cmp.cc	DBL0L,DBL1L
	.balign 4
.Lhighdiff:
	or	r12,DBL0H,DBL1H
	or.f	0,DBL0L,DBL1L
	j_s.d	[blink]
	bmsk.eq.f r12,r12,30
	ENDFUNC(__eqdf2)
/* ??? could we do better by speeding up some 'common' case of inequality?  */
