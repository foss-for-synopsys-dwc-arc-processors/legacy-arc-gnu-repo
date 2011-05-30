 /* syscall for uClibc/ARC
  *
  * Copyright (C) 2002 by Erik Andersen <andersen@uclibc.org>
  * Copyright (C) 2003 by Synopsys, Inc. (www.synopsys.com)
  *
  * This program is free software; you can redistribute it and/or modify it
  * under the terms of the GNU Library General Public License as published by
  * the Free Software Foundation; either version 2 of the License, or (at your
  * option) any later version.
  *
  * This program is distributed in the hope that it will be useful, but WITHOUT
  * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  * for more details.
  *
  * You should have received a copy of the GNU Library General Public License
  * along with this program; if not, write to the Free Software Foundation,
  * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
  */
 
 #include <features.h>
 #include <errno.h>
 #include <sys/types.h>
 #include <sys/syscall.h>
 
 #include <bits/sysnum.h>
 #include <linux/autoconf.h>

#if 1
 long syscall(long sysnum, long a, long b, long c, long d, long e, long f)
 {
 
#ifndef CONFIG_ARCH_ARC700
         register long _r0 asm("r0")=(long)(sysnum);
         register long _r6 asm("r6")=(long)(f);
         register long _r5 asm("r5")=(long)(e);
         register long _r4 asm("r4")=(long)(d);
         register long _r3 asm("r3")=(long)(c);
         register long _r2 asm("r2")=(long)(b);
         register long _r1 asm("r1")=(long)(a);
  
        asm volatile ("1: \n\t"     \
              "lr     %0, [%2] \n\t" \
              "mov.f  0, %0 \n\t"    \
              "nop    \n\t"          \
              "bnz    1b \n\t"       \
                                     \
              /* FIXME: do we need to save the sp here? */             \
              "mov %0, %3\n\t"       /* save the syscall type in r0 */ \
                                                                       \
              "sr %1, [%2] \n\t"     /* do the interrupt (syscall) */  \
                                                           \
              : "=r" (_r0)                                 \
              : "i" (SYSCALL_IRQ), "i" (AUX_IRQ_HINT),    \
		       "r" (sysnum) \
                 );
 /* not doing anything right now... I don't think this file is needed for our ARCH */
 
         if(_r0 >=(unsigned long) -4095) {
                 (*__errno_location())=(-_r0);
                 _r0=(unsigned long) -1;
         }
         return (long) _r0;
#else
         register long _r8 asm("r8")=(long)(sysnum);
         register long _r5 asm("r5")=(long)(f);
         register long _r4 asm("r4")=(long)(e);
         register long _r3 asm("r3")=(long)(d);
         register long _r2 asm("r2")=(long)(c);
         register long _r1 asm("r1")=(long)(b);
         register long _r0 asm("r0")=(long)(a);
   
       asm volatile ( 
		/* r0 - r5 already in as the they are the param regs */
		"mov	r8, %1\n\t"
		"trap0\n\t"     /* do the interrupt (syscall) */  
		: "=r" (_r0)
		: "r" (sysnum), "r" (_r0), "r" (_r1), "r" (_r2), "r" (_r3), "r" (_r4), "r" (_r5)
		);

 /* not doing anything right now... I don't think this file is needed for our ARCH */
 
         if(_r0 >=(unsigned long) -4095) {
                 (*__errno_location())=(-_r0);
                 _r0=(unsigned long) -1;
         }
         return (long) _r0;
#endif
}
#endif
