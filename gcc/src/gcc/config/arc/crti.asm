#  This file contains the stack frame setup for contents of the .fini and
# .init sections.

		
	.section .init
	.balign 4
	.global __init
	.word 0
__init:
	push_s	blink

	.section .fini
	.balign 4
	.global __fini
	.word 0
__fini:
	push_s	blink
