#include "arc-ieee-754.h"
/* inputs: r0, r1
   output: c flag
   clobber: r12, flags
   For NaNs, bit 22 .. bit 30 must be set.  */
#if 0 /* DEBUG */
	.global __ordsf2
	.balign 4
	FUNC(__ordsf2)
__ordsf2:
	st.a r11,[sp,-4]` push_s blink` st.a r10,[sp,-4]` st.a r9,[sp,-4]
	st.a r8,[sp,-4]` st.a r7,[sp,-4]` st.a r6,[sp,-4]` st.a r5,[sp,-4]
	st.a r4,[sp,-4]` push_s r3` push_s r2` push_s r1`
	bl.d __unordsf2_c` push_s r0
	mov r11,r0` pop_s r0` pop_s r1` pop_s r2` pop_s r3
	ld.ab r4,[sp,4]` ld.ab r5,[sp,4]` ld.ab r6,[sp,4]`
	ld.ab r7,[sp,4]` ld.ab r8,[sp,4]` ld.ab r9,[sp,4]
	bl.d __ordsf2_asm` ld.ab r10,[sp,4]
	pop_s blink
	brne.d r11,0,0f
	ld.ab r11,[sp,4]
	jcc [blink]
	bl abort
0:	jcs [blink]
	bl abort
	ENDFUNC(__ordsf2)
#define __ordsf2 __ordsf2_asm
#endif /* DEBUG */
	.global __ordsf2
	.balign 4
	FUNC(__ordsf2)
__ordsf2:
	bmsk r12,r0,23
	add1.f	r12,r12,r0 /* clear z; set c if NaN.  */
	bmsk r12,r1,23
	j_s.d	[blink]
	add1.cc.f r12,r12,r1 /* clear z; set c if NaN.  */
	ENDFUNC(__ordsf2)
