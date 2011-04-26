/* Syscall Macro Generator for ARC 
 * Originally written by Codeito, who in turn borrowed it from ARM/???
 *
 * vineet.gupta@viragelogic.com: Dec 1st 2009
 * Rewrote ARC700 syscall wrapper generator macro.
 *
 * 1. Got rid of all the superfluous register loads by specifying the args
 *    as asm("r-nn") as they are already in right order
 *
 * 2. Instead of updating errno inline, shunted it to a function to
 *    reduce code in EACH wrapper.
 *    push/pop for blink done only if branch actually taken.
 *
 * 3. Removed the 2 nop insns after swi as ARC700 doesn't have any such reqmt
 *
 * 4. No dependency on mar errno defined by kernel. A signed -ve number 
 *    (top bit set) is considered kernel returned error. 
 *    That way we dont need to follow kernel errno limits and also
 *    avoid a -ve long immediate from code (typically 4 bytes) 
 *
 * Note the memory clobber is not strictly needed for intended semantics of
 * the inline asm. However some of the cases, such as old-style 6 arg mmap
 * gcc was generating code for inline syscall ahead of buffer packing needed
 * for syscall itself.
 */
#ifndef _BITS_SYSCALLS_H
#define _BITS_SYSCALLS_H
#ifndef _SYSCALL_H
# error "Never use <bits/syscalls.h> directly; include <sys/syscall.h> instead."
#endif

#include <features.h>
#include <bits/sysnum.h>
#ifndef __ASSEMBLER__
#include <errno.h>
#endif


#ifdef __ARCH_HAS_C_SYMBOL_PREFIX__
#define ARC_SYMBOL_NAME(x) _##x
#define ARC_SYMBOL_ALIAS(x) ARC_SYMBOL_NAME(x) = x
#else
#define ARC_SYMBOL_NAME(x) x
#define ARC_SYMBOL_ALIAS(x)
#endif

/* Sys call name to number */
#ifndef SYS_ify
# define SYS_ify(syscall_name)	(__NR_##syscall_name)
#endif

/*
   Some of the sneaky macros in the code were taken from 
   glibc-2.2.5/sysdeps/unix/sysv/linux/arm/sysdep.h
*/

#ifndef __ASSEMBLER__

/* vineetg: Dec 1st 2009
 * To shut gcc up when building sys call wrappers.
 * Conditional because ldso defines it as a macro.
 */
#ifndef __syscall_error
extern int __syscall_error(int err_no);
#endif

#undef INLINE_SYSCALL
#undef INLINE_SYSCALL_WITH_NUM

#define INLINE_SYSCALL(name, nr, args...)				        \
    INLINE_SYSCALL_WITH_NUM(nr, SYS_ify(name), args)

/*-------------------------------------------------------------------------
  ARC700 Core syscall wrapper generator
--------------------------------------------------------------------------*/
#ifdef __CONFIG_ARCH_ARC_A7__


#ifdef IS_IN_rtld

#define ERRNO_ERRANDS(_sys_result) (0)

#else /* !IS_IN_rtld */

#ifdef __PIC__
#define CALL_ERRNO_SETTER   "bl   __syscall_error@plt    \n\t"
#else
#define CALL_ERRNO_SETTER   "bl   __syscall_error    \n\t"
#endif

#define ERRNO_ERRANDS(_sys_result)          \
        asm volatile (                      \
        "st.a blink, [sp, -4] \n\t"         \
        CALL_ERRNO_SETTER                   \
        "ld.ab blink, [sp, 4] \n\t"         \
        :"+r" (_sys_result)                 \
        :                                   \
        :"r1","r2","r3","r4","r5","r6",     \
         "r7","r8","r9","r10","r11","r12"   \
        );

#endif /* IS_IN_rtld */


#define INLINE_SYSCALL_WITH_NUM(nr, syscallnum, args...)    \
({                                                          \
    register int _sys_result asm("r0");                     \
	LOAD_ARGS_##nr (args)					        	    \
									                        \
        asm volatile (							            \
		"mov r8, %1     \n\t"       /* sys-call number */	\
		"trap0          \n\t"       /* Trap into kernel */	\
         : "+r" (_sys_result)                               \
	     : "ir" (syscallnum)  ASM_ARGS_##nr                 \
         : "r8","memory" );							        \
                                                            \
    /* -1 to -1023 as valid error values will suffice       \
            for some time */                                \
    if(_sys_result > (unsigned int) -1024)  {               \
        ERRNO_ERRANDS(_sys_result);                         \
     }                                                      \
    _sys_result;                                            \
})

/* Macros for setting up inline asm input regs */
#define ASM_ARGS_0
#define ASM_ARGS_1	ASM_ARGS_0, "r" (_sys_result)
#define ASM_ARGS_2	ASM_ARGS_1, "r" (_r1)
#define ASM_ARGS_3	ASM_ARGS_2, "r" (_r2)
#define ASM_ARGS_4	ASM_ARGS_3, "r" (_r3)
#define ASM_ARGS_5	ASM_ARGS_4, "r" (_r4)
#define ASM_ARGS_6	ASM_ARGS_5, "r" (_r5)
#define ASM_ARGS_7	ASM_ARGS_6, "r" (_r6)

/* Macros for converting sys-call wrapper args into sys call args
 * Note that this is just for sanity check as they are BOUND to be in same
 * order. The wrapper args by defn is to match sys-call args order.
 * e.g.
 * _syscall4( int, syscallname, vineet, rajesh, simon, mitesh)
 *  will cause the compiler to setup r0 = vineet, r1 = rajesh etc before
 *  invoking the sys-call wrapper. Thus args are already in ABI specified
 *  order for sys call
 */
#define LOAD_ARGS_0(arg)
#define LOAD_ARGS_1(r0) _sys_result = (int) (r0);

#define LOAD_ARGS_2(r0, r1)			\
  register int _r1 asm ("r1") = (int) (r1);	\
  LOAD_ARGS_1 (r0)

#define LOAD_ARGS_3(r0, r1, r2)			\
  register int _r2 asm ("r2") = (int) (r2);	\
  LOAD_ARGS_2 (r0, r1)

#define LOAD_ARGS_4(r0, r1, r2, r3)		\
  register int _r3 asm ("r3") = (int) (r3);	\
  LOAD_ARGS_3 (r0, r1, r2)

#define LOAD_ARGS_5(r0, r1, r2, r3, r4)		\
  register int _r4 asm ("r4") = (int) (r4);	\
  LOAD_ARGS_4 (r0, r1, r2, r3)

#define LOAD_ARGS_6(r0, r1, r2, r3, r4, r5)	\
  register int _r5 asm ("r5") = (int) (r5);	\
  LOAD_ARGS_5 (r0, r1, r2, r3, r4)

#define LOAD_ARGS_7(r0, r1, r2, r3, r4, r5, r6)	\
  register int _r6 asm ("r6") = (int) (r6);	\
  LOAD_ARGS_6 (r0, r1, r2, r3, r4, r5)

#else  /* ! __CONFIG_ARCH_ARC_A7__ */
/*-------------------------------------------------------------------------
  non ARC700
--------------------------------------------------------------------------*/

#define INLINE_SYSCALL_WITH_NUM(nr, syscallnum, args...)  \
({ unsigned int _sys_result;						\
    {									\
	register int _r0 asm ("r0");					\
	LOAD_ARGS_##nr (args);						\
									\
	/* First, check if the AUX_HINT_REG is 0... ie, no interrupt	\
	 * is currently pending/being serviced				\
	 */								\
        asm volatile (							\
		"1:     \n\t"						\
		"lr     r26, [%1] \n\t"					\
		"mov.f  0, r26 \n\t"					\
		"nop    \n\t"						\
		"bnz    1b \n\t"					\
									\
		/* FIXME: do we need to save the sp here? */		\
		"mov r0, %2 \n\t"   /* save the syscall type in r0 */	\
									\
		"sr %0, [%1] \n\t"  /* do the interrupt (syscall) */	\
		"nop \n\t" /* two nops to flush pipeline (hw issue) */	\
		"nop \n\t"						\
									\
	     : 								\
	     : "i" (SYSCALL_IRQ), "i" (AUX_IRQ_HINT),			\
	       "ir" (syscallnum)) ASM_ARGS_##nr			\
	     : "r0","memory");							\
									\
	_sys_result = _r0;						\
    }									\
    if(_sys_result >= (unsigned long) -125) {				\
	        __set_errno(-_sys_result);				\
	        _sys_result = (unsigned long) -1;			\
    }									\
	(int) _sys_result; })


#define LOAD_ARGS_0()
#define ASM_ARGS_0
#define LOAD_ARGS_1(r1)				\
  register int _r1 asm ("r1") = (int) (r1);	\
  LOAD_ARGS_0 ()
#define ASM_ARGS_1	ASM_ARGS_0, "r" (_r1)
#define LOAD_ARGS_2(r1, r2)			\
  register int _r2 asm ("r2") = (int) (r2);	\
  LOAD_ARGS_1 (r1)
#define ASM_ARGS_2	ASM_ARGS_1, "r" (_r2)
#define LOAD_ARGS_3(r1, r2, r3)			\
  register int _r3 asm ("r3") = (int) (r3);	\
  LOAD_ARGS_2 (r1, r2)
#define ASM_ARGS_3	ASM_ARGS_2, "r" (_r3)
#define LOAD_ARGS_4(r1, r2, r3, r4)		\
  register int _r4 asm ("r4") = (int) (r4);	\
  LOAD_ARGS_3 (r1, r2, r3)
#define ASM_ARGS_4	ASM_ARGS_3, "r" (_r4)
#define LOAD_ARGS_5(r1, r2, r3, r4, r5)		\
  register int _r5 asm ("r5") = (int) (r5);	\
  LOAD_ARGS_4 (r1, r2, r3, r4)
#define ASM_ARGS_5	ASM_ARGS_4, "r" (_r5)
#define LOAD_ARGS_6(r1, r2, r3, r4, r5, r6)	\
  register int _r6 asm ("r6") = (int) (r6);	\
  LOAD_ARGS_5 (r1, r2, r3, r4, r5)
#define ASM_ARGS_6	ASM_ARGS_5, "r" (_r6)
#define LOAD_ARGS_7(r1, r2, r3, r4, r5, r6, r7)	\
  register int _r7 asm ("r7") = (int) (r7);	\
  LOAD_ARGS_6 (r1, r2, r3, r4, r5, r6)
#define ASM_ARGS_7	ASM_ARGS_6, "r" (_r7)


#endif /* __CONFIG_ARCH_ARC_A7__ */

/*-------------------------------------------------------------------------
  Macros themselves used by uClibc
--------------------------------------------------------------------------*/
#undef _syscall0
#define _syscall0(type,name) \
type name(void) \
{ \
return (type) (INLINE_SYSCALL(name, 0)); \
}

#undef _syscall1
#define _syscall1(type,name,type1,arg1) \
type name(type1 arg1) \
{ \
return (type) (INLINE_SYSCALL(name, 1, arg1)); \
}

#undef _syscall2
#define _syscall2(type,name,type1,arg1,type2,arg2) \
type name(type1 arg1,type2 arg2) \
{ \
return (type) (INLINE_SYSCALL(name, 2, arg1, arg2)); \
}

#undef _syscall3
#define _syscall3(type,name,type1,arg1,type2,arg2,type3,arg3) \
type name(type1 arg1,type2 arg2,type3 arg3) \
{ \
return (type) (INLINE_SYSCALL(name, 3, arg1, arg2, arg3)); \
}

#undef _syscall4
#define _syscall4(type,name,type1,arg1,type2,arg2,type3,arg3,type4,arg4) \
type name (type1 arg1, type2 arg2, type3 arg3, type4 arg4) \
{ \
return (type) (INLINE_SYSCALL(name, 4, arg1, arg2, arg3, arg4)); \
} 

#undef _syscall5
#define _syscall5(type,name,type1,arg1,type2,arg2,type3,arg3,type4,arg4, \
	  type5,arg5) \
type name (type1 arg1,type2 arg2,type3 arg3,type4 arg4,type5 arg5) \
{ \
return (type) (INLINE_SYSCALL(name, 5, arg1, arg2, arg3, arg4, arg5)); \
}

#undef _syscall6
#define _syscall6(type,name,type1,arg1,type2,arg2,type3,arg3,type4,arg4, \
	  type5,arg5,type6,arg6) \
type name (type1 arg1,type2 arg2,type3 arg3,type4 arg4,type5 arg5, type6 arg6) \
{ \
return (type) (INLINE_SYSCALL(name, 6, arg1, arg2, arg3, arg4, arg5, arg6)); \
}

#undef _syscall7
#define _syscall7(type,name,type1,arg1,type2,arg2,type3,arg3,type4,arg4, \
	  type5,arg5,type6,arg6,type7,arg7) \
type name (type1 arg1,type2 arg2,type3 arg3,type4 arg4,type5 arg5, type6 arg6,type7 arg7) \
{ \
return (type) (INLINE_SYSCALL(name, 7, arg1, arg2, arg3, arg4, arg5, arg6, arg7)); \
}

#endif /* __ASSEMBLER__ */

#endif /* _BITS_SYSCALLS_H */
