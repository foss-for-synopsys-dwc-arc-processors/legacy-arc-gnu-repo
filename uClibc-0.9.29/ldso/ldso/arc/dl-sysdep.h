/******************************************************************************
 * Copyright Codito Technologies (www.codito.com) Oct 01, 2004
 * 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 *****************************************************************************/
/* Copyright (C) 2000-2004 by Erik Andersen <andersen@codepoet.org> */
/* Copyright (C) 2007 ARC International (UK) LTD */

/*
 * Various assmbly language/system dependent  hacks that are required
 * so that we can minimize the amount of platform specific code.
 */

#include "elf.h"

/*
 * Define this if the system uses RELOCA.
 */
#define ELF_USES_RELOCA

/*
 * Get a pointer to the argv array.  On many platforms this can be just
 * the address if the first argument, on other platforms we need to
 * do something a little more subtle here.
 */
#define GET_ARGV(ARGVP, ARGS) ARGVP = ((unsigned long*) ARGS + 1)

#define BITS(word,s,e)          (((word) << (31 - e)) >> (s + (31 -e)))
#define GOTBASE_IN_PLT0	 20	
/*
 * Initialization sequence for a GOT.The GOT Base that we always
 * receive is the DT_PLTGOT value for everybody other than the 
 * dynamic linker . Hence the check for the program interpreter in the
 * INIT_GOT macro. (The value of the GOT Base is at DT_PLTGOT (which points to the
 * plt0 entry) + 20 )
 */
#define INIT_GOT(GOT_BASE,MODULE)				   \
do {								   \
  if(MODULE->libtype != program_interpreter)		   \
	GOT_BASE = (unsigned long *)(*(unsigned long *)((char *)(GOT_BASE) + GOTBASE_IN_PLT0) + (unsigned long)MODULE->loadaddr); \
  GOT_BASE[2] = (unsigned long) _dl_linux_resolve;		   \
  GOT_BASE[1] = (unsigned long) MODULE;				   \
} while(0)

/* Added support for reading and writing middle endian format which is
   how values are stored for the ARC Compact / Tazer architecture */

static inline unsigned long arc_read_me(unsigned char *addr)
{
  unsigned long value = 0;
  value = (*addr & 255) << 16;
  value |= (*(addr + 1) & 255) << 24;
  value |= (*(addr + 2) & 255);
  value |= (*(addr + 3) & 255) << 8;
  return value;
}

static inline void arc_write_me(unsigned short *addr, unsigned long value)
{
  *addr = (value & 0xffff0000) >> 16;
  *(addr+2) = (value & 0xffff) ;
}


/* Extraction function for 25 bit relocation type for the displacement */
static inline unsigned long extract_arc_pc_rel_25(unsigned long insn)
{
  unsigned long offset ;
  offset = BITS(insn,18,26);
  offset |= BITS(insn,6,15) << 9;
  offset |= BITS(offset,0,4) << 19;
  return offset << 2;

}

/* Insertion function for 25 bit relocation type for the displacement */
static inline void insert_arc_pc_rel_25(unsigned short * addr,unsigned long offset)
{
  unsigned long insn;
  insn = arc_read_me ((unsigned char *)addr);
  offset = offset >> 2;
  insn |= BITS(offset,0,8) << 16;
  insn |= BITS(offset,9,19) << 6;
  insn |= BITS(offset,19,23);
  arc_write_me(addr,insn);
}


/*
 * Here is a macro to perform a relocation.  This is only used when
 * bootstrapping the dynamic loader.  RELP is the relocation that we
 * are performing, REL is the pointer to the address we are relocating.
 * SYMBOL is the symbol involved in the relocation, and LOAD is the
 * load address.
 */


/* Take care of Middle endian format in Bootstrap relocation */



#define PERFORM_BOOTSTRAP_RELOC(RELP,REL,SYMBOL,LOAD,SYMTAB)	\
do{                                                             \
unsigned long insn;                                             \
	insn = arc_read_me ((unsigned char *)REL);		\
	switch(ELF32_R_TYPE((RELP)->r_info)){			\
	case R_ARC_32_ME:					\
          insn += SYMBOL;                                       \
          arc_write_me ((unsigned short *)REL, insn);		\
	  break;						\
        case R_ARC_S25W_PCREL:                                  \
         insn = extract_arc_pc_rel_25(insn);                    \
         insn += SYMBOL - (unsigned long) REL;                  \
         insert_arc_pc_rel_25 ((unsigned short *)REL, insn);	\
         break;                                                 \
	case R_ARC_GLOB_DAT:					\
          *REL = SYMBOL;      		                        \
	  break;						\
	case R_ARC_JMP_SLOT:					\
          *REL = SYMBOL;      		                        \
	  break;						\
	case R_ARC_RELATIVE:					\
	   *REL += (unsigned long) LOAD;			\
	   break;						\
	default:						\
	  _dl_exit(1);						\
	}                                                       \
}while(0)


/*
 * Transfer control to the user's application, once the dynamic loader
 * is done.  This routine has to exit the current function, then 
 * call the _dl_elf_main function.
 */
#define START()	return _dl_elf_main 
/*\
	       __asm__ volatile ("j [%0] \n\t"			\
		       :"=r" (__dl_elf_main))
*/
/* Here we define the magic numbers that this dynamic loader should accept */

#define MAGIC1 EM_ARCOMPACT
#undef  MAGIC2
/* Used for error messages */
#define ELF_TARGET "ARC"

struct elf_resolve;
extern unsigned long _dl_linux_resolver(struct elf_resolve * tpnt, int reloc_entry);

static inline unsigned long arc_modulus(unsigned long m, unsigned long p) {
	unsigned long i,t,inc;
        i=p; t=0;
        while(!(i&(1<<31))) {
                i<<=1;
                t++;
        }
        t--;
        for(inc=t;inc>2;inc--) {
                i=p<<inc;
                if(i&(1<<31))
                        break;
                while(m>=i) {
                        m-=i;
                        i<<=1;
                        if(i&(1<<31))
                                break;
                        if(i<p)
                                break;
                }
        }
        while(m>=p) {
                m-=p;
        }
        return m;
}

extern unsigned __udivmodsi4 (unsigned, unsigned)
  __attribute ((visibility("hidden")));

#define do_rem(result, n, base)  \
  ((result) \
   = (__builtin_constant_p (base) \
      ? (n) % (unsigned) (base) \
      : ({ \
	   register unsigned r1 __asm__ ("r1") = (base); \
 \
	   __asm("bl.d @__udivmodsi4` mov r0,%1" : "=r" (r1) \
	         : "r" (n), "r" (r1) \
	         : "r0", "r2", "r3", "r4", "lp_count", "blink", "cc"); \
	   r1; })))

#if 0
#define COPY_HASH(TPNT, HASH_ADDR) \
do { \
  int tmp0, tmp1, tmp2;
  asm("\
	ld.ab %0,[%[addr],4] /* nbucket */ \
	ld.ab %1,[%[addr],4] /* nchain */ \
	st %[addr],%[elf_buckets] \
	norm %2,%0 \
	st %0,%[nbucket] \
	asl %2,%0,%2 \
	st %1,%[nchain] \
	divaw %0,0x40000000,%2 \
	.rep 14 \
	divaw %0,%0,%2 \
	.endr \
	add %[addr],%[addr],%1 \
	bmsk %1,%0,14 \
	bic %0,%0,%1 \
	.rep15 \
	divaw %0,%0,%2 \
	.endr \
	asl %1,%1,15 \
	bmsk %0,%0,14 \
	add %0,%0,%1 \
	add %0,%0,1 \
	st %[addr],%[chains] \
	st %[nbucket_inv],%0" \
	: "=&r" (tmp0), "=&r" (tmp1), "=&r" (tmp2), \
	  [addr] "+r" (HASH_ADDR), [nbucket] "=m" ((TPNT)->nbucket), \
	  [nchain] "=m" ((TPNT)->nchain), \
	  [elf_buckets] "=m" ((TPNT)->elf_buckets), \
	  [chains] "=m" ((TPNT)->chains), \
	  [nbucket_inv] "=m" ((TPNT)->nbucket_inv)); \
} while (0)

#define ELF_RESOLVE_TDEP unsigned long nbucket_inv;
#endif

/* ELF_RTYPE_CLASS_PLT iff TYPE describes relocation of a PLT entry, so
   PLT entries should not be allowed to define the value.
   ELF_RTYPE_CLASS_NOCOPY iff TYPE should not be allowed to resolve to one
   of the main executable's symbols, as for a COPY reloc.  */
#define elf_machine_type_class(type) \
  ((((type) == R_ARC_JMP_SLOT) * ELF_RTYPE_CLASS_PLT)	\
   | (((type) == R_ARC_COPY) * ELF_RTYPE_CLASS_COPY))

/* Return the link-time address of _DYNAMIC.  Conveniently, this is the
   first element of the GOT.  This must be inlined in a function which
   uses global data.  */
static inline Elf32_Addr elf_machine_dynamic (void) attribute_unused;
static inline Elf32_Addr
elf_machine_dynamic (void)
{
        register Elf32_Addr *got __asm__ ("gp");
        return *got;
}

/* Return the run-time load address of the shared object.  */
static inline Elf32_Addr elf_machine_load_address (void) attribute_unused;
static inline Elf32_Addr
elf_machine_load_address (void)
{
  /* It doesn't matter what variable this is, the reference never makes
     it to assembly.  We need a dummy reference to some global variable
     via the GOT to make sure the compiler initialized %ebx in time.  */
  extern int _dl_errno;
  Elf32_Addr addr, tmp;
  __asm__ (
#ifdef GAS_FIXED
	   "ld %1,[pcl,_dl_start@gotpc]\n\t"
#else
	   "add %1,pcl,_dl_start@gotpc\n\t"
	   "ld %1,[%1]\n\t"
#endif
	   "add %0,gp,_dl_start@gotoff\n\t"
	   "sub %0,%0,%1"
         : "=c" (addr), "=r" (tmp) : "m" (_dl_errno));
  return addr;
}

static inline void
elf_machine_relative (Elf32_Addr load_off, const Elf32_Addr rel_addr,
		      Elf32_Word relative_count)
{
	 Elf32_Rel * rpnt = (void *) rel_addr;
	--rpnt;
	do {
		Elf32_Addr *const reloc_addr = (void *) (load_off + (++rpnt)->r_offset);

		*reloc_addr += load_off;
	} while (--relative_count);
}
