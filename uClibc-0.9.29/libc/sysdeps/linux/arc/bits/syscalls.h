#ifndef _BITS_SYSCALLS_H
#define _BITS_SYSCALLS_H
#ifndef _SYSCALL_H
# error "Never use <bits/syscalls.h> directly; include <sys/syscall.h> instead."
#endif

/* This includes the `__NR_<name>' syscall numbers taken from the Linux kernel
 * header files.  It also defines the traditional `SYS_<name>' macros for older
 * programs.  */
#include <features.h>
#include <bits/sysnum.h>
#include <asm/unistd.h>
#include <asm/system.h>

#ifdef __ARCH_HAS_C_SYMBOL_PREFIX__
#define ARC_SYMBOL_NAME(x) _##x
#define ARC_SYMBOL_ALIAS(x) ARC_SYMBOL_NAME(x) = x
#else
#define ARC_SYMBOL_NAME(x) x
#define ARC_SYMBOL_ALIAS(x)
#endif

#ifndef __ASSEMBLER__
#include <errno.h>
#if 0
#ifndef __set_errno
# define __set_errno(val) (*__errno_location ()) = (val)
#endif
#endif
#endif
#ifndef SYS_ify
# define SYS_ify(syscall_name)	(__NR_##syscall_name)
#endif

/*
   Some of the sneaky macros in the code were taken from 
   glibc-2.2.5/sysdeps/unix/sysv/linux/arm/sysdep.h
*/

#ifndef __ASSEMBLER__


#undef INLINE_SYSCALL

//-------------------------------------------------------------------------
#ifdef __CONFIG_ARCH_ARC_A7__

#define INLINE_SYSCALL0(name, nr, args...)				\
({ unsigned int _sys_result;						    \
    {								                 	\
	LOAD_ARGS_##nr (args)					        	\
									                    \
        asm volatile (							        \
		/* FIXME: do we need to save the sp here? */	\
		"mov r8, %1 \n\t"   /* save the syscall type in r8 */	\
		"trap0 \n\t"  /* do the interrupt (syscall) */	        \
		"nop \n\t" /* two nops to flush pipeline (hw issue) */	\
		"nop \n\t"						                        \
        "mov %0, r0 \n\t"                                       \
	     : "=r" (_sys_result)						            \
	     : "i" (SYS_ify(name)) ASM_ARGS_##nr			        \
	     : "r0","r8");							                \
    }									                        \
    if(_sys_result >= (unsigned long) -125) {				    \
	        __set_errno(-_sys_result);			            	\
	        _sys_result = (unsigned long) -1;		        	\
    }								                        	\
	(int) _sys_result; })

#define INLINE_SYSCALL(name, nr, args...)				\
({ unsigned int _sys_result;						    \
    {								                 	\
	LOAD_ARGS_##nr (args)					        	\
									                    \
        asm volatile (							        \
		/* FIXME: do we need to save the sp here? */	\
		"mov r8, %1 \n\t"   /* save the syscall type in r8 */	\
		"mov r0, %2 \n \t" \
		"trap0 \n\t"  /* do the interrupt (syscall) */	        \
		"nop \n\t" /* two nops to flush pipeline (hw issue) */	\
		"nop \n\t"						                        \
        "mov %0, r0 \n\t"                                       \
	     : "=r" (_sys_result)						            \
	     : "i" (SYS_ify(name)), "r"(_r0) ASM_ARGS_##nr			        \
	     : "r0","r8");							                \
    }									                        \
    if(_sys_result >= (unsigned long) -125) {				    \
	        __set_errno(-_sys_result);			            	\
	        _sys_result = (unsigned long) -1;		        	\
    }								                        	\
	(int) _sys_result; })

#define LOAD_ARGS_0()	\
	register int _r0  = (int) 0;
#define ASM_ARGS_0
#define LOAD_ARGS_1(r0)				\
  LOAD_ARGS_0 ()				\
	_r0  = (int) (r0);	
//#define ASM_ARGS_1	ASM_ARGS_0, "r" (_r0) 
#define ASM_ARGS_1	ASM_ARGS_0
#define LOAD_ARGS_2(r0, r1)			\
  register int _r1 asm ("r1") = (int) (r1);	\
  LOAD_ARGS_1 (r0)
#define ASM_ARGS_2	ASM_ARGS_1, "r" (_r1)
#define LOAD_ARGS_3(r0, r1, r2)			\
  register int _r2 asm ("r2") = (int) (r2);	\
  LOAD_ARGS_2 (r0, r1)
#define ASM_ARGS_3	ASM_ARGS_2, "r" (_r2)
#define LOAD_ARGS_4(r0, r1, r2, r3)		\
  register int _r3 asm ("r3") = (int) (r3);	\
  LOAD_ARGS_3 (r0, r1, r2)
#define ASM_ARGS_4	ASM_ARGS_3, "r" (_r3)
#define LOAD_ARGS_5(r0, r1, r2, r3, r4)		\
  register int _r4 asm ("r4") = (int) (r4);	\
  LOAD_ARGS_4 (r0, r1, r2, r3)
#define ASM_ARGS_5	ASM_ARGS_4, "r" (_r4)
#define LOAD_ARGS_6(r0, r1, r2, r3, r4, r5)	\
  register int _r5 asm ("r5") = (int) (r5);	\
  LOAD_ARGS_5 (r0, r1, r2, r3, r4)
#define ASM_ARGS_6	ASM_ARGS_5, "r" (_r5)
#define LOAD_ARGS_7(r0, r1, r2, r3, r4, r5, r6)	\
  register int _r6 asm ("r6") = (int) (r6);	\
  LOAD_ARGS_6 (r0, r1, r2, r3, r4, r5)
#define ASM_ARGS_7	ASM_ARGS_6, "r" (_r6)

//----------------------------------------------------------------------------------------
#else

#define INLINE_SYSCALL(name, nr, args...)				\
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
	       "i" (SYS_ify(name)) ASM_ARGS_##nr			\
	     : "r0");							\
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


#endif /* CONFIG_ARCH_ARC_A7 */

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
