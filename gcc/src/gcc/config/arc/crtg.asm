#  This file contains code to start and stop profiling.
#ifndef __A4__

	.section .init
	.global __init
	.global __fini
	.global __monstartup
	mov_s	r0,__init
	mov_s	r1,__fini
	jl	__monstartup

	.section .__arc_profile_desc, "a"
	.global __arc_profile_desc_secstart
	.balign	4
__arc_profile_desc_secstart:
	.section .__arc_profile_forward, "a"
	.global __arc_profile_forward_secstart
	.balign 4
__arc_profile_forward_secstart:
	.section .__arc_profile_counters, "aw"
	.global __arc_profile_counters_secstart
	.balign	4
__arc_profile_counters_secstart:

	.section .fini
	.global _mcleanup
	jl	_mcleanup
#endif /* !A4 */
