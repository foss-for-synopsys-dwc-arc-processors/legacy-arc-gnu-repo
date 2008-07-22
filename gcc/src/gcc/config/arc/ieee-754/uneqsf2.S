#include "arc-ieee-754.h"
/* inputs: r0, r1
   output: z flag
   clobber: r12, flags
   For NaNs, bit 22 .. bit 30 must be set.  */
#if 0 /* DEBUG */
	.global __uneqsf2
	.balign 4
	FUNC(__uneqsf2)
__uneqsf2:
	st.a r11,[sp,-4]` push_s blink` st.a r10,[sp,-4]` st.a r9,[sp,-4]
	st.a r8,[sp,-4]` st.a r7,[sp,-4]` st.a r6,[sp,-4]` st.a r5,[sp,-4]
	st.a r4,[sp,-4]` push_s r3` push_s r2` push_s r1`
	bl.d __eqsf2_c` push_s r0
	push_s r0` ld_s r0, [sp,4]
	bl.d __unordsf2_c` ld_s r1,[sp,8]
	ld.ab r11,[sp,4]` tst r0,r0` mov.ne r11,0
	pop_s r0` pop_s r1` pop_s r2` pop_s r3
	ld.ab r4,[sp,4]` ld.ab r5,[sp,4]` ld.ab r6,[sp,4]`
	ld.ab r7,[sp,4]` ld.ab r8,[sp,4]` ld.ab r9,[sp,4]
	bl.d __uneqsf2_asm` ld.ab r10,[sp,4]
	pop_s blink
	breq.d r11,0,0f
	ld.ab r11,[sp,4]
	jne_s [blink]
	bl abort
0:	jeq_s [blink]
	bl abort
	ENDFUNC(__uneqsf2)
#define __uneqsf2 __uneqsf2_asm
#endif /* DEBUG */
	.global __uneqsf2
	.balign 4
	HIDDEN_FUNC(__uneqsf2)
__uneqsf2:
	mov_s	r12, \
		0x7fc00000
	bic.f	0,r12,r0
	bic.ne.f r12,r12,r1
	or	r12,r0,r1
	bmsk.ne.f r12,r12,30
	j_s.d	[blink]
	cmp.ne	r0,r1
	ENDFUNC(__uneqsf2)
