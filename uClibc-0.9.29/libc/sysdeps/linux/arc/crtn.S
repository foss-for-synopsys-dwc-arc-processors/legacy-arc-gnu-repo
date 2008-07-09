___gnu_compiled_c:
	
	.section .init
	.align 4
	.global	__init
	.type	 __init,@function
	; EPILOGUE
#ifdef __A4__
	ld	fp, [sp]
	add	sp, sp, 4
#else
	ld.ab fp,[sp,4]
#endif
	ld blink,[sp,0]
	j.d [blink]
	add sp,sp,4
.Lfe2:
	.size	 __init,.Lfe2-__init
	
	.section .fini
	.align 4
	.global	__fini
	.type	 __fini,@function
	; EPILOGUE
#ifdef __A4__
	ld	fp, [sp]
	add	sp, sp, 4
#else
	ld.ab fp,[sp,4]
#endif
	ld blink,[sp,0]
	j.d [blink]
	add sp,sp,4
.Lfe3:
	.size	 __fini,.Lfe3-__fini
	
