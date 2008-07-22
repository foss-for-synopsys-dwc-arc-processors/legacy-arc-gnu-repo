/* Definitions of target machine for GNU compiler, Argonaut ARC cpu.
   Copyright (C) 1994, 1995, 1997, 1998 Free Software Foundation, Inc.

   Sources derived from work done by Sankhya Technologies (www.sankhya.com)

   Position Independent Code support added,Code cleaned up, 
   Comments and Support For ARC700 instructions added by
   Saurabh Verma (saurabh.verma@codito.com)
   Ramana Radhakrishnan(ramana.radhakrishnan@codito.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_ARC_H
#define GCC_ARC_H

/* Things to do:

   - incscc, decscc?
	
*/

/* ************************************************************************* 
 * Role of the SYMBOL_REF_FLAG in the rtx:
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * This is to document the change in the role of the SYMBOL_REF_FLAG
 * from the PIC enabled version of the toolchain onwards.
 * Before the PIC support was added to the compiler, the machine
 * specific SYMBOL_REF_FLAG was used to mark a function's symbol so
 * that a symbol reference to a function ( as in obtaining a pointer to
 * that function ) is printed as %st(<<functionname>>).
 *
 *   The PIC version of the compiler uses this flag to mark a locally
 * defined symbol, for which references in the code have to be made as
 * pc+ symbolname@GOTOFF instead of symbolname@GOT.Also references to
 * local functions are made relative instead of going through the PLT.
 *      The earlier work of the flag is accomplished by mangling the
 * name of the symbol(adding an *_CALL_FLAG_CHAR at the start) and modifying 
 * the print_operand routine to unmangle it and print the reference as
 * %st(symbol_name_unmangled) instead. The convention used for mangling 
 * accomodates the long_call and short_call function attributes by using one
 * of (LONG_/SHORT_/SIMPLE_)CALL_FLAG_CHAR characters as the prefix.
 * ************************************************************************/


/* ashwin : include options.h */
/* #include "options.h" */

#undef ASM_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC
#undef ENDFILE_SPEC
#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#undef ASM_APP_ON
#undef ASM_APP_OFF
#undef CC1_SPEC

/* Print subsidiary information on the compiler version in use.  */
#ifndef USE_UCLIBC
#define TARGET_VERSION fprintf (stderr, " (arc)")
#else
#define TARGET_VERSION fprintf (stderr, " (ARC GNU Linux / uClibc with ELF)")
#endif



/* Names to predefine in the preprocessor for this target machine.  */
/*
   ??? check whether __base__ definition can be removed. If it can be
       removed, "#ifdef __base__" has to be removed from lib1funcs.asm.
*/
#define TARGET_CPU_CPP_BUILTINS()	\
 do {					\
    builtin_define("__arc__");		\
    builtin_define("__base__");		\
    if (TARGET_SIMD_SET)        	\
      builtin_define("__ARC_SIMD__");	\
    builtin_assert("cpu=arc");		\
    builtin_assert("machine=arc");	\
    builtin_define (TARGET_BIG_ENDIAN	\
		    ? "__BIG_ENDIAN__" : "__LITTLE_ENDIAN__"); \
    if (TARGET_BIG_ENDIAN)		\
      builtin_define ("__big_endian__"); \
} while(0)

/* Additional flags for the preprocessor.  */
#ifndef USE_UCLIBC
#define CPP_SPEC "\
%{mA4:-D__A4__} %{mA5:-D__A5__} %{mA6:-D__A6__ -D__ARC600__} %{mA7:-D__A7__ -D__ARC700__} \
%{mARC600:-D__A6__ -D__ARC600__} %{mARC700:-D__A7__ -D__ARC700__} \
%{mmixed-code|!mA4:%{!mA5:%{!mA6:%{!mARC600:%{!mA7:%{!mARC700:-D__A5__}}}}}} \
"

#else
#define TARGET_CPU_arc700 2

#define CPP_SPEC "\
%{mA5:-D__A5__} %{mA6:-D__A6__ -D__ARC600} %{mA7:-D__A7__ -D__ARC700__} \
%{mARC600:-D__A6__ -D__ARC600__} %{mARC700:-D__A7__ -D__ARC700__} \
%{!mA4:%{!mA5:%{!mA6:%{!mARC600:%{!mA7:%{!mARC700:%{!mmixed-code:-D__A5__}}}}}}} \
%{mmixed-code:%{!mA5:%{!mA6:%{!mARC600:%{!mA7:%{!mARC700:-D__A5__}}}}}} \
"

#endif 

/* Pass -mmangle-cpu if we get -mA* : Not applicable any more.
   Doing it this way lets one have it on as default with -mA*,
   but also lets one turn it off with -mno-mangle-cpu.  */
#define CC1_SPEC "\
%{EB:%{EL:%emay not use both -EB and -EL}} \
%{EB:-mbig-endian} %{EL:-mlittle-endian} \
"
#ifndef USE_UCLIBC

#define ASM_SPEC  "\
%{v} %{EB} %{EL} %{mA4} %{mA5} %{mA6} %{mARC600} %{mA7} %{mARC700} \
%{mbarrel_shifter} %{mno-mpy} %{mmul64} %{mnorm} %{mswap} %{mARC700|mA7:-mEA} %{mEA} %{mmin_max} %{mspfp*} %{mdpfp*} \
%{msimd} %{mmixed-code|!mA4:%{!mA5:%{!mA6:%{!mARC600:%{!mA7:%{!mARC700:-mA5}}}}}}" 
#else

#ifndef TARGET_CPU_arc700
#define ASM_SPEC  "\
%{v} %{mbig-endian:-EB} %{EB} %{EL} %{mA4} %{mA5} %{mA6} %{mARC600} %{mA7} %{mARC700} \
%{mbarrel_shifter} %{mno-mpy} %{mmul64} %{mnorm} %{mswap} %{mARC700|mA7:-mEA} %{mEA} %{mmin_max} %{mspfp*} %{mdpfp*} \
%{msimd} %{mmixed-code:%{!mA5:%{!mA6:%{!mARC600:%{!mA7:%{!mARC700:-mA5}}}}}}" 
#else

#define ASM_SPEC  "\
%{v} %{mbig-endian:-EB} %{EB} %{EL} %{mA4} %{mA5} %{mA6} %{mARC600} %{mA7} %{mARC700} \
%{mbarrel_shifter} %{mno-mpy} %{mmul64} %{mnorm} %{mswap} %{mARC700|mA7:-mEA} %{mEA} %{mmin_max} %{mspfp*} %{mdpfp*} \
%{msimd} %{mmixed-code:%{-mA7}}" 

#endif
#endif

#ifdef USE_UCLIBC
#if 0
/* Note that the default is to link against dynamic libraries, if they
   available.  While it is a bit simpler to get started with static linking,
   it is much easier to comply with the LGPL when you use dynamic linking, and
   thus get a product that you can legally ship.  */
#define LINK_SPEC "%{h*} %{version:-v} \
                   %{b} %{Wl,*:%*}     \
                   %{static:-Bstatic}  \
                   %{symbolic:-Bsymbolic} \
                   %{rdynamic:-export-dynamic}\
                   %{!dynamic-linker:-dynamic-linker /lib/ld-uClibc.so.0}\
                   -X %{mbig-endian:-EB} \
                   %{EB} %{EL} \
		   %{pg|p|profile:-marclinux_prof;: -marclinux}"
#else /* Make ease of use of producing something the main concern.  */
#define LINK_SPEC "%{h*} %{version:-v} \
                   %{b} %{Wl,*:%*}     \
                   %{!mdynamic:%{!shared:-Bstatic}}  \
                   %{symbolic:-Bsymbolic} \
                   %{rdynamic:-export-dynamic}\
                   %{!dynamic-linker:-dynamic-linker /lib/ld-uClibc.so.0}\
                   -X %{mbig-endian:-EB} \
                   %{EB} %{EL} \
                   %{shared:-shared}\
		   %{pg|p|profile:-marclinux_prof;: -marclinux}"
#endif
#else
#define LINK_SPEC "%{v} %{mbig-endian:-EB} %{EB} %{EL}\
  %{pg|p:-marcelf_prof;mA7|mARC700: -marcelf}"
#endif

#ifndef USE_UCLIBC
#define STARTFILE_SPEC "%{!shared:crt0.o%s} crti%O%s %{pg|p:crtg.o%s} crtbegin.o%s"
#else
#define STARTFILE_SPEC   "%{!shared:%{!mkernel:crt1.o%s}} crti.o%s \
  %{!shared:%{pg|p|profile:crtg.o%s} crtbegin.o%s} %{shared:crtbeginS.o%s}"

#endif

#ifndef USE_UCLIBC
#define ENDFILE_SPEC "%{pg|p:crtgend.o%s} crtend.o%s crtn%O%s"
#else
#define ENDFILE_SPEC "%{!shared:%{pg|p|profile:crtgend.o%s} crtend.o%s} \
  %{shared:crtendS.o%s} crtn.o%s"

#endif 

#ifdef USE_UCLIBC
#undef LIB_SPEC
#define LIB_SPEC  \
  "%{pthread:-lpthread} \
   %{shared:-lc} \
   %{!shared:%{pg|p|profile:-lgmon -u profil --defsym __profil=profil} -lc}"
#else
#undef LIB_SPEC
/* -lc_p not present for arc-elf32-* : ashwin */
#define LIB_SPEC "%{!shared:%{g*:-lg} %{pg|p:-lgmon} -lc}"
#endif


#ifdef USE_UCLIBC
#define DRIVER_SELF_SPECS "%{!mA*:-mA7}"
#endif

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

#define TARGET_ARCOMPACT  (TARGET_A5 || TARGET_ARC600 || TARGET_ARC700)
#define TARGET_MIXED_CODE (TARGET_ARCOMPACT && TARGET_MIXED_CODE_SET)

#define TARGET_SPFP (TARGET_SPFP_FAST_SET || TARGET_SPFP_COMPACT_SET)
#define TARGET_DPFP (TARGET_DPFP_FAST_SET || TARGET_DPFP_COMPACT_SET)

#define SUBTARGET_SWITCHES

/* Instruction set characteristics.
   These are internal macros, set by the appropriate -m option.  */

/* Non-zero means the cpu has a barrel shifter. This flag is set by default
 * for post A4 cores, and only for A4 when -mbarrel_shifter is given.  */
#define TARGET_SHIFTER (TARGET_ARCOMPACT || TARGET_BARREL_SHIFTER_SET)

/* Non-zero means the cpu supports norm instruction.  This flag is set by
 * default for A7, and only for pre A7 cores when -mnorm is given.  */
#define TARGET_NORM (TARGET_ARC700 || TARGET_NORM_SET)

/* Non-zero means the cpu supports swap instruction.  This flag is set by
 * default for A7, and only for pre A7 cores when -mswap is given.  */
#define TARGET_SWAP (TARGET_ARC700 || TARGET_SWAP_SET)

/* Non-zero means the cpu supports min and max instructions.  This flag is set by
 * default for post A4 cores, and only for A4 when -mmin_max is given.  */
#define TARGET_MINMAX (TARGET_ARCOMPACT || TARGET_MINMAX_SET)

enum processor_type {
  PROCESSOR_A4,
  PROCESSOR_A5,
  PROCESSOR_ARC600,
  PROCESSOR_ARC700
};  
 
extern enum processor_type arc_cpu;  /* which cpu we are compiling for */
extern const char *arc_cpu_string;   /* A4/A5/ARC600/ARC700 */
extern short int mixed_code_enabled; /* Flag to identify whether mixed
                                        code can be generated */

/* ashwin : since TARGET_OPTIONS are moved to arc.opt, no need of these here */
/* extern const char *arc_text_string,*arc_data_string,*arc_rodata_string; */

/* Recast the cpu class to be the cpu attribute.  */
#define arc_cpu_attr ((enum attr_cpu)arc_cpu)

/* Check if CPU is an extension and set `arc_mangle_cpu' appropriately.
   The result should be non-zero if the cpu is recognized,
   otherwise zero.  This is intended to be redefined in a cover file.
   This is used by arc_init.  */
#define ARC_EXTENSION_CPU(cpu) 0

#ifndef MULTILIB_DEFAULTS
#ifndef USE_UCLIBC
#define MULTILIB_DEFAULTS { "mA5", "EL" }
#else
#define MULTILIB_DEFAULTS { "mARC700", "EL" }
#endif
#endif

#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)                                \
do {                                                                    \
  arc_size_opt_level = (SIZE) ? 3 : (LEVEL >= 3) ? 0 : 1;		\
  flag_no_common = -1; /* Mark as not user-initialized.  */		\
} while (0)

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

#define OVERRIDE_OPTIONS \
do {				\
  if (arc_size_opt_level == 3)	\
    optimize_size = 1;		\
  if (flag_pic) \
    target_flags |= MASK_NO_SDATA_SET; \
  if (flag_no_common == -1)	\
    flag_no_common = !TARGET_NO_SDATA_SET; \
  /* These need to be done at start up.  It's convenient to do them here.  */ \
  arc_init ();			\
} while (0)

/* Target machine storage layout.  */

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
/*#define REAL_ARITHMETIC*/

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.

   --> In big-endian mode, the compiler allocates bit fields from the
       most significant bit to the least significant bit.
   --> In little-endian mode, the compiler allocates bit fields from the
       least significant bit to the most significant bit.
*/
#define BITS_BIG_ENDIAN (TARGET_BIG_ENDIAN)

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN)

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN)

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#ifdef __big_endian__
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#endif

/* Number of bits in an addressable storage unit.  */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */
#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE) \
if (GET_MODE_CLASS (MODE) == MODE_INT		\
    && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)	\
{						\
  (MODE) = SImode;				\
}

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
/* TOCHECK: Changed from 64 to 32 */
#define STACK_BOUNDARY 32

/* ALIGN FRAMES on word boundaries */
#define ARC_STACK_ALIGN(LOC) (((LOC)+7) & ~7)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* An expression for the alignment of a structure field FIELD if the
   alignment computed in the usual way (including applying of
   `BIGGEST_ALIGNMENT' and `BIGGEST_FIELD_ALIGNMENT' to the
   alignment) is COMPUTED.  It overrides alignment only if the field
   alignment has not been set by the `__attribute__ ((aligned (N)))'
   construct.
*/

#define ADJUST_FIELD_ALIGN(FIELD, COMPUTED) \
(TYPE_MODE (TREE_CODE (TREE_TYPE (FIELD)) == ARRAY_TYPE \
	    ? get_inner_array_type (FIELD) \
	    : TREE_TYPE (FIELD)) == DFmode \
 ? MIN ((COMPUTED), 32) : (COMPUTED))



/* No data type wants to be aligned rounder than this.  */
/* This is bigger than currently necessary for the ARC.  If 8 byte floats are
   ever added it's not clear whether they'll need such alignment or not.  For
   now we assume they will.  We can always relax it if necessary but the
   reverse isn't true.  */
/* TOCHECK: Changed from 64 to 32 */
#define BIGGEST_ALIGNMENT 32

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))


/* Make arrays of chars word-aligned for the same reasons.  */
#define LOCAL_ALIGNMENT(TYPE, ALIGN)             \
  (TREE_CODE (TYPE) == ARRAY_TYPE               \
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode    \
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && arc_size_opt_level < 3			\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
/* On the ARC the lower address bits are masked to 0 as necessary.  The chip
   won't croak when given an unaligned address, but the insn will still fail
   to produce the correct result.  */
#define STRICT_ALIGNMENT 1

/* Layout of source language data types.  */

#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		32
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64
#define LONG_DOUBLE_TYPE_SIZE	64

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

#define SIZE_TYPE "long unsigned int"
#define PTRDIFF_TYPE "long int"
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16


/* ashwin : shifted from arc.c:102 */
#define PROGRAM_COUNTER_REGNO 63

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   Registers 61, 62, and 63 are not really registers and we needn't treat
   them as such.  We still need a register for the condition code and
   argument pointer */

/* r63 is pc, r64-r127 = simd vregs, r128-r143 = simd dma config regs
   r144, r145 = lp_start, lp_end
   and therefore the pseudo registers start from r146 */
#define FIRST_PSEUDO_REGISTER 146

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   0-28  - general purpose registers
   29    - ilink1 (interrupt link register)
   30    - ilink2 (interrupt link register)
   31    - blink (branch link register)
   32-59 - reserved for extensions
   60    - LP_COUNT
   61    - condition code
   62    - argument pointer
   63    - program counter

   For doc purposes:
   61    - short immediate data indicator (setting flags)
   62    - long immediate data indicator
   63    - short immediate data indicator (not setting flags).

   The general purpose registers are further broken down into:

   0-7   - arguments/results
   8-12  - call used (r11 - static chain pointer)
   13-25 - call saved
   26    - global pointer
   27    - frame pointer
   28    - stack pointer
   29    - ilink1
   30    - ilink2
   31    - return address register

   By default, the extension registers are not available.  */
/* Size of this and value of FIRST_PSEUDO_REGISTER should be equal, *\/ */
/* hence the added another '1' at the end */
/*    reference : init_reg_sets() in regclass.c:290 */
/* Present implementations only have VR0-VR23 only */
/* ??? FIXME: r27 and r31 should not be fixed registers.  */
#define FIXED_REGISTERS \
{ 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 1, 1, 1, 1, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 0, 1, 1, 1,       \
				\
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

/* Size of this and value of FIRST_PSEUDO_REGISTER should be equal, *\/ */
/* hence the added another '1' at the end */
/*    reference : init_reg_sets() in regclass.c:290 */
#define CALL_USED_REGISTERS     \
{                               \
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 1, 1, 1, 1, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
  1, 1, 1, 1, 1, 1, 1, 1,       \
				\
  0, 0, 0, 0, 0, 0, 0, 0,       \
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1}

/* Macro to conditionally modify fixed_regs/call_used_regs.  */

#define CONDITIONAL_REGISTER_USAGE                     \
{                                                      \
  int regno;						\
							\
  if (TARGET_MIXED_CODE)                               \
    {                                                  \
      reg_alloc_order[2] = 12;                         \
      reg_alloc_order[3] = 13;                         \
      reg_alloc_order[4] = 14;                         \
      reg_alloc_order[5] = 15;                         \
      reg_alloc_order[6] = 1;                          \
      reg_alloc_order[7] = 0;                          \
      reg_alloc_order[8] = 4;                          \
      reg_alloc_order[9] = 5;                          \
      reg_alloc_order[10] = 6;                         \
      reg_alloc_order[11] = 7;                         \
      reg_alloc_order[12] = 8;                         \
      reg_alloc_order[13] = 9;                         \
      reg_alloc_order[14] = 10;                        \
      reg_alloc_order[15] = 11;                        \
    }                                                  \
    if (TARGET_SIMD_SET)                               \
    {                                                  \
      int i;					       \
      for (i=64; i<88; i++)			       \
	reg_alloc_order [i] = i;		       \
    }						       \
  /* For Arctangent-A5 / ARC600, lp_count may not be	\
     read in an instruction following immediately after	\
     another one setting it to a new value.		\
     Till we can ensure this property is kept, disable	\
     lp_count use.  */					\
  if (!TARGET_ARC700)					\
    fixed_regs[LP_COUNT] = 1;				\
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++) \
    if (!call_used_regs[regno])				\
      CLEAR_HARD_REG_BIT (reg_class_contents[SIBCALL_REGS], regno); \
}

/* If defined, an initializer for a vector of integers, containing the
   numbers of hard registers in the order in which GNU CC should
   prefer to use them (from most preferred to least).  */
#define REG_ALLOC_ORDER \
{ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0, 1,			\
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 				\
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,	\
  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62,		\
  27, 28, 29, 30, 31, 63}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */
#define HARD_REGNO_NREGS(REGNO, MODE) \
((GET_MODE_SIZE (MODE) == 16 && REGNO>=64 && REGNO<88)?1:                             \
          (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.  */
extern unsigned int arc_hard_regno_mode_ok[];
extern unsigned int arc_mode_class[];
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
((arc_hard_regno_mode_ok[REGNO] & arc_mode_class[MODE]) != 0)

/* A C expression that is nonzero if it is desirable to choose
   register allocation so as to avoid move instructions between a
   value of mode MODE1 and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R,
   MODE2)' are ever different for any R, then `MODES_TIEABLE_P (MODE1,
   MODE2)' must be zero.  */

/* Tie QI/HI/SI modes together.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
(GET_MODE_CLASS (MODE1) == MODE_INT		\
 && GET_MODE_CLASS (MODE2) == MODE_INT		\
 && GET_MODE_SIZE (MODE1) <= UNITS_PER_WORD	\
 && GET_MODE_SIZE (MODE2) <= UNITS_PER_WORD)

/* Internal macros to classify a register number as to whether it's a
   general purpose register for compact insns (r0-r3,r12-r15), or
   stack pointer (r28).  */

#define COMPACT_GP_REG_P(REGNO) \
   (((signed)(REGNO) >= 0 && (REGNO) <= 3) || ((REGNO) >= 12 && (REGNO) <= 15))
#define SP_REG_P(REGNO)  ((REGNO) == 28)



/* Register classes and constants.  */

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.

   It is important that any condition codes have class NO_REGS.
   See `register_operand'.  */

enum reg_class 
{
   NO_REGS, 
   R0_REG,			/* 'x' */
   GP_REG,			/* 'Rgp' */
   FP_REG,			/* 'f' */
   SP_REG,			/* 'b' */
   LPCOUNT_REG, 		/* 'l' */
   LINK_REGS,	 		/* 'k' */
   DOUBLE_REGS,			/* D0, D1 */
   SIMD_VR_REGS,		/* VR00-VR63 */
   SIMD_DMA_CONFIG_REGS,	/* DI0-DI7,DO0-DO7 */
   ARCOMPACT16_REGS,		/* 'q' */
   AC16_BASE_REGS,  		/* 'e' */
   SIBCALL_REGS,		/* "Rsc" */
   GENERAL_REGS,		/* 'r' */
   CORE_REGS,			/* 'c' */
   ALL_REGS,
   LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */
#define REG_CLASS_NAMES	  \
{                         \
  "NO_REGS",           	  \
  "R0_REG",            	  \
  "GP_REG",            	  \
  "FP_REG",            	  \
  "SP_REG",		  \
  "LPCOUNT_REG",	  \
  "LINK_REGS",         	  \
  "DOUBLE_REGS",          \
  "SIMD_VR_REGS",         \
  "SIMD_DMA_CONFIG_REGS", \
  "ARCOMPACT16_REGS",  	  \
  "AC16_BASE_REGS",       \
  "SIBCALL_REGS",	  \
  "GENERAL_REGS",      	  \
  "CORE_REGS",		  \
  "ALL_REGS"          	  \
} 

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS \
{													\
  {0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000},	     /* No Registers */			\
  {0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000},      /* 'x', r0 register , r0 */	\
  {0x04000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000},      /* 'Rgp', Global Pointer, r26 */	\
  {0x08000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000},      /* 'f', Frame Pointer, r27 */	\
  {0x10000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000},      /* 'b', Stack Pointer, r28 */	\
  {0x00000000, 0x10000000, 0x00000000, 0x00000000, 0x00000000},      /* 'l', LPCOUNT Register, r60 */	\
  {0xe0000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000},      /* 'k', LINK Registers, r29-r31 */	\
  {0x00000000, 0x00000f00, 0x00000000, 0x00000000, 0x00000000},      /* 'D', D1, D2 Registers */	\
  {0x00000000, 0x00000000, 0xffffffff, 0xffffffff, 0x00000000},      /* 'V', VR00-VR63 Registers */	\
  {0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000ffff},      /* 'V', DI0-7,DO0-7 Registers */	\
  {0x0000f00f, 0x00000000, 0x00000000, 0x00000000, 0x00000000},	     /* 'q', r0-r3, r12-r15 */		\
  {0x1000f00f, 0x00000000, 0x00000000, 0x00000000, 0x00000000},	     /* 'e', r0-r3, r12-r15, sp */	\
  {0x1c001fff, 0x10000000, 0x00000000, 0x00000000, 0x00000000},    /* "Rsc", r0-r12 and lp_count */ \
  {0x9fffffff, 0xc0000000, 0x00000000, 0x00000000, 0x00000000},      /* 'r', r0-r28, blink, ap and pcl */	\
  {0xffffffff, 0xdfffffff, 0x00000000, 0x00000000, 0x00000000},      /* 'c', r0-r60, ap, pcl */ \
  {0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x0003ffff}       /* All Registers */		\
}

/* local macros to mark the first and last regs of different classes */
#define ARC_FIRST_SIMD_VR_REG              64
#define ARC_LAST_SIMD_VR_REG               127

#define ARC_FIRST_SIMD_DMA_CONFIG_REG      128
#define ARC_FIRST_SIMD_DMA_CONFIG_IN_REG   128
#define ARC_FIRST_SIMD_DMA_CONFIG_OUT_REG  136
#define ARC_LAST_SIMD_DMA_CONFIG_REG       143

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern enum reg_class arc_regno_reg_class[];

#define REGNO_REG_CLASS(REGNO) (arc_regno_reg_class[REGNO])

/* The class value for valid index registers. An index register is 
   one used in an address where its value is either multiplied by 
   a scale factor or added to another register (as well as added to a
   displacement).  */

#define INDEX_REG_CLASS (TARGET_MIXED_CODE ? ARCOMPACT16_REGS : GENERAL_REGS)

/* The class value for valid base registers. A base register is one used in
   an address which is the register value plus a displacement.  */

#define BASE_REG_CLASS (TARGET_MIXED_CODE ? AC16_BASE_REGS : GENERAL_REGS)

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
#define REGNO_OK_FOR_BASE_P(REGNO) \
((REGNO) < 29 || ((REGNO) == ARG_POINTER_REGNUM) || ((REGNO) == 63) ||\
 (unsigned) reg_renumber[REGNO] < 29)

#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_BASE_P(REGNO)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS) \
  ((CLASS) == CORE_REGS ? GENERAL_REGS \
   : (CLASS))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE) \
(( GET_MODE_SIZE (MODE) == 16 && CLASS == SIMD_VR_REGS) ? 1: \
((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* local to this file */
#define SMALL_INT(X) ((unsigned) ((X) + 0x100) < 0x200)
#define SMALL_INT_RANGE(X, OFFSET, SHIFT) \
  ((unsigned) (((X) >> (SHIFT)) + 0x100) \
   < 0x200 - ((unsigned) (OFFSET) >> (SHIFT)))
#define SIGNED_INT12(X) ((unsigned) ((X) + 0x800) < 0x1000)
/* local to this file */
#define LARGE_INT(X) \
((X) >= (-(HOST_WIDE_INT) 0x7fffffff - 1) \
 && (unsigned HOST_WIDE_INT) (X) <= (unsigned HOST_WIDE_INT) 0xffffffff)
#define UNSIGNED_INT3(X) ((unsigned) (X) < 0x8)
#define UNSIGNED_INT5(X) ((unsigned) (X) < 0x20)
#define UNSIGNED_INT6(X) ((unsigned) (X) < 0x40)
#define UNSIGNED_INT7(X) ((unsigned) (X) < 0x80)
#define UNSIGNED_INT8(X) ((unsigned) (X) < 0x100)
#define IS_ONE(X) ((X) == 1)
#define IS_ZERO(X) ((X) == 0)

/* Stack layout and stack pointer usage.  */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
/* ashwin : need to give some value to this macro */
#define FRAME_GROWS_DOWNWARD 1

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET (TARGET_A4 ? 16 : 0)

/* Offset of first parameter from the argument pointer register value.  */
/* For ARCtangent-A4:
    first_parm_offset = fp_size (4 bytes) + return_addr_size (4 bytes) +
                        static_link_reg_size (4 bytes) +
                        reserved_area_size (4 bytes);
   For ARCompact:
    first_parm_offset = 0;
*/
#define FIRST_PARM_OFFSET(FNDECL) (TARGET_A4 ? 16 : 0)

/* A C expression whose value is RTL representing the address in a
   stack frame where the pointer to the caller's frame is stored.
   Assume that FRAMEADDR is an RTL expression for the address of the
   stack frame itself.

   If you don't define this macro, the default is to return the value
   of FRAMEADDR--that is, the stack frame address is also the address
   of the stack word that points to the previous frame.  */
/* ??? unfinished */
/*define DYNAMIC_CHAIN_ADDRESS (FRAMEADDR)*/

/* A C expression whose value is RTL representing the value of the
   return address for the frame COUNT steps up from the current frame.
   FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME'
   is defined.  */
/* The current return address is in r31.  The return address of anything
   farther back is at [%fp,4].  */

#define RETURN_ADDR_RTX(COUNT, FRAME) \
arc_return_addr_rtx(COUNT,FRAME)

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 28

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 27

/* Base register for access to arguments of the function. This register
   will be eliminated into either fp or sp. */
#define ARG_POINTER_REGNUM 62

#define RETURN_ADDR_REGNUM 31

/* TODO - check usage of STATIC_CHAIN_REGNUM with a testcase */
/* Register in which static-chain is passed to a function.  This must
   not be a register used by the prologue.  */
#define STATIC_CHAIN_REGNUM  11

/* A C expression which is nonzero if a function must have and use a
   frame pointer.  This expression is evaluated in the reload pass.
   If its value is nonzero the function will have a frame pointer.  */
#define FRAME_POINTER_REQUIRED \
(current_function_calls_alloca)


/* Function argument passing.  */

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */
#define RETURN_POPS_ARGS(DECL, FUNTYPE, SIZE) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */
#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT,N_NAMED_ARGS) \
((CUM) = 0)

/* The number of registers used for parameter passing.  Local to this file.  */
#define MAX_ARC_PARM_REGS 8

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N) \
((unsigned) (N) < MAX_ARC_PARM_REGS)

/* The ROUND_ADVANCE* macros are local to this file.  */
/* Round SIZE up to a word boundary.  */
#define ROUND_ADVANCE(SIZE) \
(((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round arg MODE/TYPE up to the next word boundary.  */
#define ROUND_ADVANCE_ARG(MODE, TYPE) \
((MODE) == BLKmode				\
 ? ROUND_ADVANCE (int_size_in_bytes (TYPE))	\
 : ROUND_ADVANCE (GET_MODE_SIZE (MODE)))

/* Round CUM up to the necessary point for argument MODE/TYPE.  */
/* N.B. Vectors have alignment exceeding BIGGEST_ALIGNMENT.
   FUNCTION_ARG_BOUNDARY reduces this to no more than 64 bit.  */
#define ROUND_ADVANCE_CUM(CUM, MODE, TYPE) \
  ((((CUM) - 1) | (FUNCTION_ARG_BOUNDARY ((MODE), (TYPE)) - 1)/BITS_PER_WORD)\
   + 1)

/* Special characters prefixed to function names
   in order to encode attribute like information. */
#define SIMPLE_CALL_FLAG_CHAR   '&'
#define SHORT_CALL_FLAG_CHAR	'!'
#define LONG_CALL_FLAG_CHAR	'#'

/* Local macros to identify the function name symbols, prefixed with one of
   the (LONG_CALL/SHORT_CALL/)FLAG_CHAR flags */
#define ARC_FUNCTION_NAME_PREFIX_P(ch) (((ch) == SIMPLE_CALL_FLAG_CHAR) ||   \
				    ((ch) == LONG_CALL_FLAG_CHAR)   ||   \
				    ((ch) == SHORT_CALL_FLAG_CHAR))

/* Check if this symbol has a long_call attribute in its declaration */
#define ARC_ENCODED_LONG_CALL_ATTR_P(SYMBOL_NAME)	\
  (*(SYMBOL_NAME) == LONG_CALL_FLAG_CHAR)

/* Check if this symbol has a short_call attribute in its declaration */
#define ARC_ENCODED_SHORT_CALL_ATTR_P(SYMBOL_NAME)	\
  (*(SYMBOL_NAME) == SHORT_CALL_FLAG_CHAR)

/* Return boolean indicating arg of type TYPE and mode MODE will be passed in
   a reg.  This includes arguments that have to be passed by reference as the
   pointer to them is passed in a reg if one is available (and that is what
   we're given).
   When passing arguments NAMED is always 1.  When receiving arguments NAMED
   is 1 for each argument except the last in a stdarg/varargs function.  In
   a stdarg function we want to treat the last named arg as named.  In a
   varargs function we want to treat the last named arg (which is
   `__builtin_va_alist') as unnamed.
   This macro is only used in this file.  */
#define PASS_IN_REG_P(CUM, MODE, TYPE) \
((CUM) < MAX_ARC_PARM_REGS)


/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */
/* On the ARC the first MAX_ARC_PARM_REGS args are normally in registers
   and the rest are pushed.  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
    arc_function_arg (&CUM, MODE, TYPE, NAMED) 

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
       arc_function_arg_advance(&CUM, MODE, TYPE, NAMED)

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined, 
   PARM_BOUNDARY is used for all arguments.  */
#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
(((TYPE) ? TYPE_ALIGN (TYPE) : GET_MODE_BITSIZE (MODE)) <= PARM_BOUNDARY \
 ? PARM_BOUNDARY \
 : 2 * PARM_BOUNDARY)


/* Function results.  */

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE) gen_rtx_REG (MODE, 0)

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */
/* ??? What about r1 in DI/DF values.  */
#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* A C expression which can inhibit the returning of certain function
   values in registers, based on the type of value.  A nonzero value says
   to return the function value in memory, just as large structures are
   always returned.  Here TYPE will be a C expression of type `tree',
   representing the data type of the value.  */
#define RETURN_IN_MEMORY(TYPE) \
(AGGREGATE_TYPE_P (TYPE) \
 || int_size_in_bytes (TYPE) > 8 \
 || TREE_ADDRESSABLE (TYPE))

/* Tell GCC to use RETURN_IN_MEMORY.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Register in which address to store a structure value
   is passed to a function, or 0 to use `invisible' first argument.  */
#define STRUCT_VALUE 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 0

/* Epilogue delay slots.  */
#define DELAY_SLOTS_FOR_EPILOGUE arc_delay_slots_for_epilogue ()

#define ELIGIBLE_FOR_EPILOGUE_DELAY(TRIAL, SLOTS_FILLED) \
arc_eligible_for_epilogue_delay (TRIAL, SLOTS_FILLED)

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   We have two registers that can be eliminated on the ARC.  First, the
   argument pointer register can always be eliminated in favor of the stack
   pointer register or frame pointer register.  Secondly, the frame pointer
   register can often be eliminated in favor of the stack pointer register.
*/

#define ELIMINABLE_REGS					\
{{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},		\
 {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   All eliminations are permissible. If we need a frame
   pointer, we must eliminate ARG_POINTER_REGNUM into
   FRAME_POINTER_REGNUM and not into STACK_POINTER_REGNUM.  */
#define CAN_ELIMINATE(FROM, TO)         \
  (!((FROM) == FRAME_POINTER_REGNUM && FRAME_POINTER_REQUIRED))

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
extern int arc_initial_elimination_offset(int from, int to);
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)                    \
  (OFFSET) = arc_initial_elimination_offset ((FROM), (TO))

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.
   We actually emit the profiler code at the call site, so leave this one
   empty.  */
#define FUNCTION_PROFILER(FILE, LABELNO)
#define NO_PROFILE_COUNTERS  1

/* Trampolines.  */
#if 0 /* ??? What is that?  */
/* ??? This doesn't work yet because GCC will use as the address of a nested
   function the address of the trampoline.  We need to use that address
   right shifted by 2.  It looks like we'll need PSImode after all. :-(  */

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */
/* On the ARC, the trampoline is quite simple as we have 32 bit immediate
   constants.

	mov r24,STATIC
	j.nd FUNCTION
*/
#define TRAMPOLINE_TEMPLATE(FILE) \
do { \
  ASM_OUTPUT_INT (FILE, GEN_INT (0x631f7c00)); \
  ASM_OUTPUT_INT (FILE, const0_rtx); \
  ASM_OUTPUT_INT (FILE, GEN_INT (0x381f0000)); \
  ASM_OUTPUT_INT (FILE, const0_rtx); \
} while (0)
#endif /* weird stuff */

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE 20

/* Alignment required for a trampoline in bits .  */
/* For actual data alignment we just need 32, no more than the stack;
   however, to reduce cache coherency issues, we want to make sure that
   trampoline instructions always appear the same in any given cache line.  */
#define TRAMPOLINE_ALIGNMENT 256

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */
/* ashwin : All gen_rtx (VAR, ..) have been converted to gen_rtx_VAR (..) except */
/* gen_rtx (code, ..) which is converted to gen_rtx_fmt_ee (code..) */
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
  arc_initialize_trampoline (TRAMP, FNADDR, CXT)

/* Allow the profiler to easily distinguish trampolines from normal
  functions.  */
#define TRAMPOLINE_ADJUST_ADDRESS(addr) ((addr) = plus_constant ((addr), 2))

/* Library calls.  */

/* Addressing modes, and classification of registers for them.  */

/* Maximum number of registers that can appear in a valid memory address.  */
/* The `ld' insn allows 2, but the `st' insn only allows 1.  */
#define MAX_REGS_PER_ADDRESS 1

/* We have pre inc/dec (load/store with update).  */
#define HAVE_PRE_INCREMENT 1
#define HAVE_PRE_DECREMENT 1
#define HAVE_POST_INCREMENT 1
#define HAVE_POST_DECREMENT 1
#define HAVE_PRE_MODIFY_DISP 1
#define HAVE_POST_MODIFY_DISP 1
#define HAVE_PRE_MODIFY_REG 1
#define HAVE_POST_MODIFY_REG 1
/* ??? should also do PRE_MODIFY_REG / POST_MODIFY_REG, but that requires
   a special predicate for the memory operand of stores, like for the SH.  */

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X) \
(flag_pic?arc_legitimate_pic_addr_p (X): \
(GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF	\
 || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST))

/* Nonzero if the constant value X is a legitimate general operand.
   We can handle any 32 or 64 bit constant.  */
/* "1" should work since the largest constant should be a 64 bit critter.  */
/* ??? Not sure what to do for 64x32 compiler.  */
#define LEGITIMATE_CONSTANT_P(X) (arc_legitimate_constant_p (X))

/* Is the argument a const_int rtx, containing an exact power of 2 */
#define  IS_POWEROF2_P(X) (! ( (X) & ((X) - 1)))

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Source files for reload pass need to be strict.
   After reload, it makes no difference, since pseudo regs have
   been eliminated by then.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) \
((unsigned) REGNO (X) >= FIRST_PSEUDO_REGISTER || \
 (unsigned) REGNO (X) < 29 || \
 (unsigned) REGNO (X) == 63 || \
 (unsigned) REGNO (X) == ARG_POINTER_REGNUM)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
((unsigned) REGNO (X) >= FIRST_PSEUDO_REGISTER || \
 (unsigned) REGNO (X) < 29 || \
 (unsigned) REGNO (X) == 63 || \
 (unsigned) REGNO (X) == ARG_POINTER_REGNUM)

#else
/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.  */
/* The `ld' insn allows [reg],[reg+shimm],[reg+limm],[reg+reg],[limm]
   but the `st' insn only allows [reg],[reg+shimm],[limm].
   The only thing we can do is only allow the most strict case `st' and hope
   other parts optimize out the restrictions for `ld'.  */

/* local to this file */
#define RTX_OK_FOR_BASE_P(X) \
(REG_P (X) && REG_OK_FOR_BASE_P (X))

/* local to this file */
#define RTX_OK_FOR_INDEX_P(X) \
(REG_P (X) && REG_OK_FOR_INDEX_P (X))

/* local to this file */
/* ??? Loads can handle any constant, stores can only handle small ones.  */
/* OTOH, LIMMs cost extra, so their usefulness is limited.  */
#define RTX_OK_FOR_OFFSET_P(MODE, X) \
(GET_CODE (X) == CONST_INT           \
 && SMALL_INT_RANGE (INTVAL (X), (GET_MODE_SIZE (MODE) - 1) & -4, \
		     (INTVAL (X) & (GET_MODE_SIZE (MODE) - 1) & 3 \
		      ? 0 \
		      : -(-GET_MODE_SIZE (MODE) | -4) >> 1)))

#define LEGITIMATE_OFFSET_ADDRESS_P(MODE, X, INDEX) \
(GET_CODE (X) == PLUS			     \
  && RTX_OK_FOR_BASE_P (XEXP (X, 0))         \
  && ((INDEX && RTX_OK_FOR_INDEX_P (XEXP (X, 1)) \
       && GET_MODE_SIZE ((MODE)) <= 4) \
      || RTX_OK_FOR_OFFSET_P (MODE, XEXP (X, 1))))
    
#define LEGITIMATE_SCALED_ADDRESS_P(MODE, X) \
(GET_CODE (X) == PLUS \
 && GET_CODE (XEXP (X, 0)) == MULT \
 && RTX_OK_FOR_INDEX_P (XEXP (XEXP (X, 0), 0)) \
 && GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT \
 && ((GET_MODE_SIZE (MODE) == 2 && INTVAL (XEXP (XEXP (X, 0), 1)) == 2) \
     || (GET_MODE_SIZE (MODE) == 4 && INTVAL (XEXP (XEXP (X, 0), 1)) == 4)) \
 && (RTX_OK_FOR_BASE_P (XEXP (X, 1)) \
     || (flag_pic ? CONST_INT_P (XEXP (X, 1)) : CONSTANT_P (XEXP (X, 1)))))

#define LEGITIMATE_SMALL_DATA_ADDRESS_P(X) \
(GET_CODE (X) == PLUS			     \
 && (REG_P (XEXP(X,0)) && REGNO (XEXP (X,0)) == 26)           \
&& ((GET_CODE (XEXP(X,1)) == SYMBOL_REF \
 && SYMBOL_REF_SMALL_P (XEXP (X,1)))  ||\
 (GET_CODE (XEXP (X,1)) == CONST && \
  GET_CODE (XEXP(XEXP(X,1),0)) == PLUS && \
  GET_CODE (XEXP(XEXP(XEXP(X,1),0),0)) == SYMBOL_REF \
  && SYMBOL_REF_SMALL_P (XEXP(XEXP (XEXP(X,1),0),0)) \
  && GET_CODE (XEXP(XEXP (XEXP(X,1),0), 1)) == CONST_INT)))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)			\
{ if (RTX_OK_FOR_BASE_P (X))					\
     goto ADDR;							\
  if (LEGITIMATE_OFFSET_ADDRESS_P ((MODE), (X), TARGET_INDEXED_LOADS)) \
     goto ADDR;							\
  if (LEGITIMATE_SCALED_ADDRESS_P ((MODE), (X)))		\
    goto ADDR;							\
  if (LEGITIMATE_SMALL_DATA_ADDRESS_P (X))			\
     goto ADDR;							\
  if (GET_CODE (X) == CONST_INT && LARGE_INT (INTVAL (X)))	\
     goto ADDR;							\
  if ( (GET_MODE_SIZE (MODE) != 16) &&                               \
      (GET_CODE (X) == SYMBOL_REF				\
      || GET_CODE (X) == LABEL_REF				\
      || GET_CODE (X) == CONST))				\
  if (!flag_pic || arc_legitimate_pic_addr_p (X))		\
     goto ADDR;							\
  if ((GET_CODE (X) == PRE_DEC || GET_CODE (X) == PRE_INC	\
       || GET_CODE (X) == POST_DEC || GET_CODE (X) == POST_INC)	\
      && RTX_OK_FOR_BASE_P (XEXP ((X), 0)))			\
    goto ADDR;							\
      /* We're restricted here by the `st' insn.  */		\
  if ((GET_CODE (X) == PRE_MODIFY || GET_CODE (X) == POST_MODIFY) \
      && GET_CODE (XEXP ((X), 1)) == PLUS			\
      && rtx_equal_p (XEXP ((X), 0), XEXP (XEXP ((X), 1), 0))	\
      && LEGITIMATE_OFFSET_ADDRESS_P (QImode, XEXP ((X), 1),	\
				      TARGET_AUTO_MODIFY_REG))	\
    goto ADDR;							\
}

#define SYMBOLIC_CONST(X)	\
(GET_CODE (X) == SYMBOL_REF						\
 || GET_CODE (X) == LABEL_REF						\
 || (GET_CODE (X) == CONST && symbolic_reference_mentioned_p (X)))

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */
#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)				\
{									\
  if (flag_pic && SYMBOLIC_CONST (X)) 					\
     (X) =  arc_legitimize_pic_address (X, 0);			\
  if (memory_address_p (MODE, X))					\
     goto WIN;								\
}

/* A C compound statement that attempts to replace X, which is an address
   that needs reloading, with a valid memory address for an operand of
   mode MODE.  WIN is a C statement label elsewhere in the code.

   We try to get a normal form
   of the address.  That will allow inheritance of the address reloads.  */

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)	\
{									\
  if (GET_CODE (X) == PLUS						\
      && CONST_INT_P (XEXP (X, 1))					\
      && RTX_OK_FOR_BASE_P (XEXP (X, 0)))				\
    {									\
      int scale = GET_MODE_SIZE (MODE);					\
      int shift;							\
      rtx index_rtx = XEXP (X, 1);					\
      HOST_WIDE_INT offset = INTVAL (index_rtx), offset_base;		\
      rtx sum;								\
									\
      if (scale > 4)							\
	scale = 4;							\
      if ((scale-1) & offset)						\
	scale = 1;							\
      shift = scale >> 1;						\
      offset_base = (offset + (256 << shift)) & (-512 << shift);	\
      /* Sometimes the normal form does not suit DImode.  We		\
	 could avoid that by using smaller ranges, but that		\
	 would give less optimized code when SImode is			\
	 prevalent.  */							\
      if (GET_MODE_SIZE (MODE) + offset - offset_base <= (256 << shift))\
	{								\
	  sum = gen_rtx_PLUS (Pmode, XEXP (X, 0),			\
			      GEN_INT (offset_base));			\
	  X = gen_rtx_PLUS (Pmode, sum, GEN_INT (offset - offset_base));\
	  push_reload (sum, NULL_RTX, &XEXP (X, 0), NULL,		\
		       BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, (OPNUM),	\
		       (TYPE));						\
	  goto WIN;							\
	}								\
    }									\
  /* We must re-recognize what we created before.  */			\
  else if (GET_CODE (X) == PLUS						\
	   && GET_CODE (XEXP (X, 0)) == PLUS				\
	   && CONST_INT_P (XEXP (XEXP (X, 0), 1))			\
	   && RTX_OK_FOR_BASE_P (XEXP (XEXP (X, 0), 0))		\
	   && CONST_INT_P (XEXP (X, 1)))				\
    {									\
      /* Because this address is so complex, we know it must have	\
	 been created by LEGITIMIZE_RELOAD_ADDRESS before; thus,	\
	 it is already unshared, and needs no further unsharing.  */	\
      push_reload (XEXP ((X), 0), NULL_RTX, &XEXP ((X), 0), NULL,	\
		   BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, (OPNUM), (TYPE));\
      goto WIN;								\
    }									\
}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL) \
{ if (GET_CODE (ADDR) == PRE_DEC)	\
    goto LABEL;				\
  if (GET_CODE (ADDR) == PRE_INC)	\
    goto LABEL;				\
  if (GET_CODE (ADDR) == POST_DEC || GET_CODE (ADDR) == POST_INC) \
    goto LABEL; \
  if ((GET_CODE (ADDR) == POST_MODIFY || GET_CODE (ADDR) == POST_MODIFY) \
      && !CONST_INT_P (XEXP (XEXP ((ADDR), 1), 1))) \
    goto LABEL; \
  /* SYMBOL_REF is not mode dependent: it is either a small data reference, \
     which is valid for loads and stores, or a limm offset, which is valid for \
     loads.  */ \
  if (GET_CODE (ADDR) == PLUS		\
      && (GET_CODE (XEXP ((ADDR), 0)) == MULT \
	  || (CONST_INT_P (XEXP ((ADDR), 1)) \
	      && !SMALL_INT (INTVAL (XEXP ((ADDR), 1)))))) \
    goto LABEL;				\
}


/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */
/*extern enum machine_mode arc_select_cc_mode ();*/
#define SELECT_CC_MODE(OP, X, Y) \
arc_select_cc_mode (OP, X, Y)

/* Return non-zero if SELECT_CC_MODE will never return MODE for a
   floating point inequality comparison.  */
#define REVERSIBLE_CC_MODE(MODE) 1 /*???*/

/* Costs.  */

/* Compute extra cost of moving data between one register class
   and another.  */
#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2) 2

/* Compute the cost of moving data between registers and memory.  */
/* Memory is 3 times as expensive as registers.
   ??? Is that the right way to look at it?  */
#define MEMORY_MOVE_COST(MODE,CLASS,IN) \
(GET_MODE_SIZE (MODE) <= UNITS_PER_WORD ? 6 : 12)

/* The cost of a branch insn.  */
/* ??? What's the right value here?  Branches are certainly more
   expensive than reg->reg moves.  */
#define BRANCH_COST 2

/* Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS  0

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
/* On the ARC, calling through registers is slow.  */
#define NO_FUNCTION_CSE

/* Section selection.  */
/* WARNING: These section names also appear in dwarfout.c.  */

/* The names of the text, data, and readonly-data sections are runtime
   selectable.  */

#define ARC_SECTION_FORMAT		"\t.section %s"
#define ARC_DEFAULT_TEXT_SECTION	".text"
#define ARC_DEFAULT_DATA_SECTION	".data"
#define ARC_DEFAULT_RODATA_SECTION	".rodata"

extern const char *arc_text_section,*arc_data_section,*arc_rodata_section;

/* initfini.c uses this in an asm.  */
#if defined (CRT_INIT) || defined (CRT_FINI) || defined (CRT_BEGIN) || defined (CRT_END)
#define TEXT_SECTION_ASM_OP	"\t.section .text"
#else
#define TEXT_SECTION_ASM_OP	arc_text_section /*"\t.section .text"*/
#endif
#define DATA_SECTION_ASM_OP	arc_data_section /*"\t.section .data"*/

#undef  READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP	arc_rodata_section  /*"\t.section .rodata"*/

#define BSS_SECTION_ASM_OP	"\t.section .bss"
#define SDATA_SECTION_ASM_OP	"\t.section .sdata"
#define SBSS_SECTION_ASM_OP	"\t.section .sbss"

/* Expression whose value is a string, including spacing, containing the 
   assembler operation to identify the following data as initialization/termination
   code. If not defined, GCC will assume such a section does not exist. */
#define INIT_SECTION_ASM_OP "\t.section\t.init"
#define FINI_SECTION_ASM_OP "\t.section\t.fini"

/* Define this macro if jump tables (for tablejump insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.
   This macro is irrelevant if there is no separate readonly data section.  */
#define JUMP_TABLES_IN_TEXT_SECTION  (flag_pic)

/* For DWARF.  Marginally different than default so output is "prettier"
   (and consistent with above).  */
#define PUSHSECTION_FORMAT "\t%s %s\n"

/* Tell crtstuff.c we're using ELF.  */
#define OBJECT_FORMAT_ELF

/* PIC */

/* The register number of the register used to address a table of static
   data addresses in memory.  In some cases this register is defined by a
   processor's ``application binary interface'' (ABI).  When this macro
   is defined, RTL is generated for this register once, as with the stack
   pointer and frame pointer registers.  If this macro is not defined, it
   is up to the machine-dependent files to allocate such a register (if
   necessary).  */
#define PIC_OFFSET_TABLE_REGNUM 26

/* Define this macro if the register defined by PIC_OFFSET_TABLE_REGNUM is
   clobbered by calls.  Do not define this macro if PIC_OFFSET_TABLE_REGNUM
   is not defined.  */
/* This register is call-saved on the ARC.  */
/*#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED*/

/* A C expression that is nonzero if X is a legitimate immediate
   operand on the target machine when generating position independent code.
   You can assume that X satisfies CONSTANT_P, so you need not
   check this.  You can also assume `flag_pic' is true, so you need not
   check it either.  You need not define this macro if all constants
   (including SYMBOL_REF) can be immediate operands when generating
   position independent code.  */
#define LEGITIMATE_PIC_OPERAND_P(X)  (arc_legitimate_pic_operand_p(X))

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */
#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START(FILE) arc_asm_file_start (FILE)

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will
   end at the end of the line.  */
#define ASM_COMMENT_START ";"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */
#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */
#define ASM_APP_OFF ""

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* This is how to output an assembler line defining a `char' constant.  */
#define ASM_OUTPUT_CHAR(FILE, VALUE) \
( fprintf (FILE, "\t.byte\t"),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line defining a `short' constant.  */
#define ASM_OUTPUT_SHORT(FILE, VALUE) \
( fprintf (FILE, "\t.hword\t"),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line defining an `int' constant.
   We also handle symbol output here.  Code addresses must be right shifted
   by 2 because that's how the jump instruction wants them.
   We take care to not generate %st for post A4 cores. */
#define ASM_OUTPUT_INT(FILE, VALUE) \
do {									\
  fprintf (FILE, "\t.word\t");						\
  if (TARGET_A4 && (GET_CODE (VALUE) == SYMBOL_REF                      \
         && ARC_FUNCTION_NAME_PREFIX_P(* (XSTR (VALUE, 0))))                \
      || GET_CODE (VALUE) == LABEL_REF)					\
    {									\
      fprintf (FILE, "%%st(@");						\
      output_addr_const (FILE, (VALUE));				\
      fprintf (FILE, ")");						\
    }									\
  else									\
    output_addr_const (FILE, (VALUE));					\
  fprintf (FILE, "\n");					                \
} while (0)

/* This is how to output an assembler line defining a `float' constant.  */
#define ASM_OUTPUT_FLOAT(FILE, VALUE) \
{							\
  long t;						\
  char str[30];						\
  REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);		\
  REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);	\
  fprintf (FILE, "\t.word\t0x%lx %s %s\n",		\
	   t, ASM_COMMENT_START, str);			\
}

/* This is how to output an assembler line defining a `double' constant.  */
#define ASM_OUTPUT_DOUBLE(FILE, VALUE) \
{							\
  long t[2];						\
  char str[30];						\
  REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);		\
  REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);	\
  fprintf (FILE, "\t.word\t0x%lx %s %s\n\t.word\t0x%lx\n", \
	   t[0], ASM_COMMENT_START, str, t[1]);		\
}

/* This is how to output an assembler line for a numeric constant byte.  */
#define ASM_BYTE_OP	".byte"
#define ASM_OUTPUT_BYTE(FILE, VALUE)  \
  fprintf (FILE, "\t%s\t0x%x\n", ASM_BYTE_OP, (VALUE))

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */
#define ASM_OUTPUT_LABEL(FILE, NAME) \
do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

#define ASM_NAME_P(NAME) ( NAME[(ARC_FUNCTION_NAME_PREFIX_P (NAME[0]))?1:0]=='*')

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */
/* We mangle all user labels to provide protection from linking code
   compiled for different cpus.  */
/* We work around a dwarfout.c deficiency by watching for labels from it and
   not adding the '_' prefix nor the cpu suffix.  There is a comment in
   dwarfout.c that says it should be using ASM_OUTPUT_INTERNAL_LABEL.  */
#define ASM_OUTPUT_LABELREF(FILE, NAME1) \
do {							\
  const char *NAME;                                     \
  NAME=(*targetm.strip_name_encoding)(NAME1);           \
  if ((NAME)[0] == '.' && (NAME)[1] == 'L')		\
         fprintf (FILE, "%s", NAME);			\
  else							\
    {							\
      if (!ASM_NAME_P (NAME1))                           \
      fprintf (FILE, "%s", user_label_prefix);		\
      fprintf (FILE, "%s", NAME);			\
    }							\
} while (0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO) \
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */

#undef  TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT	"@%s"

/*  A C string containing the appropriate assembler directive to
    specify the size of a symbol, without any arguments.  On systems
    that use ELF, the default (in `config/elfos.h') is `"\t.size\t"';
    on other systems, the default is not to define this macro. */
#undef SIZE_ASM_OP
#define SIZE_ASM_OP "\t.size\t"

/* Assembler pseudo-op to equate one value with another.  */
/* ??? This is needed because dwarfout.c provides a default definition too
   late for defaults.h (which contains the default definition of ASM_OTPUT_DEF
   that we use).  */
#ifdef SET_ASM_OP
#undef SET_ASM_OP
#endif
#define SET_ASM_OP "\t.set\t"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */
#define REGISTER_NAMES								\
{  "r0",   "r1",   "r2",   "r3",       "r4",     "r5",     "r6",    "r7",	\
   "r8",   "r9",  "r10",  "r11",      "r12",    "r13",    "r14",   "r15",	\
  "r16",  "r17",  "r18",  "r19",      "r20",    "r21",    "r22",   "r23",	\
  "r24",  "r25",   "gp",   "fp",       "sp", "ilink1", "ilink2", "blink",	\
  "r32",  "r33",  "r34",  "r35",      "r36",    "r37",    "r38",   "r39",	\
   "d1",   "d1",   "d2",   "d2",      "r44",    "r45",    "r46",   "r47",	\
  "r48",  "r49",  "r50",  "r51",      "r52",    "r53",    "r54",   "r55",	\
  "r56",  "r57",  "r58",  "r59", "lp_count",     "cc",     "ap",   "pcl",	\
  "vr0",  "vr1",  "vr2",  "vr3",      "vr4",    "vr5",    "vr6",   "vr7",       \
  "vr8",  "vr9", "vr10", "vr11",     "vr12",   "vr13",   "vr14",  "vr15",	\
 "vr16", "vr17", "vr18", "vr19",     "vr20",   "vr21",   "vr22",  "vr23",	\
 "vr24", "vr25", "vr26", "vr27",     "vr28",   "vr29",   "vr30",  "vr31",	\
 "vr32", "vr33", "vr34", "vr35",     "vr36",   "vr37",   "vr38",  "vr39",	\
 "vr40", "vr41", "vr42", "vr43",     "vr44",   "vr45",   "vr46",  "vr47",	\
 "vr48", "vr49", "vr50", "vr51",     "vr52",   "vr53",   "vr54",  "vr55",	\
 "vr56", "vr57", "vr58", "vr59",     "vr60",   "vr61",   "vr62",  "vr63",	\
  "dr0",  "dr1",  "dr2",  "dr3",      "dr4",    "dr5",    "dr6",   "dr7",	\
  "dr0",  "dr1",  "dr2",  "dr3",      "dr4",    "dr5",    "dr6",   "dr7",	\
  "lp_start", "lp_end" \
}

/* Entry to the insn conditionalizer.  */
#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) \
arc_final_prescan_insn (INSN, OPVEC, NOPERANDS)

/* A C expression which evaluates to true if CODE is a valid
   punctuation character for use in the `PRINT_OPERAND' macro.  */
extern char arc_punct_chars[];
#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
arc_punct_chars[(unsigned char) (CHAR)]

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */
#define PRINT_OPERAND(FILE, X, CODE) \
arc_print_operand (FILE, X, CODE)

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   On some machines, the syntax for a symbolic address depends on
   the section that the address refers to.  On these machines,
   define the macro `ENCODE_SECTION_INFO' to store the information
   into the `symbol_ref', and then check for it here.  */
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) \
arc_print_operand_address (FILE, ADDR)

/* This is how to output an element of a case-vector that is absolute.
    We take care to not generate %st for post A4 cores. */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
do {							\
  char label[30];					\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);	\
  fprintf (FILE, "\t.word ");			        \
  if(TARGET_A4)                                         \
     fprintf(FILE, "%%st(@");                           \
  arc_assemble_name (FILE, label);			\
  if(TARGET_A4)						\
     fprintf (FILE, ")");				\
  fprintf(FILE, "\n");		                        \
} while (0)

/* This is how to output an element of a case-vector that is relative.
   We take care to not generate %st for post A4 cores. */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
do {							\
  char label[30];					\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);	\
  fprintf (FILE, "\t.word ");			        \
  if(TARGET_A4)                                         \
     fprintf(FILE, "%%st(");                            \
  arc_assemble_name (FILE, label);			\
  fprintf (FILE, "-");					\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", REL);	\
  arc_assemble_name (FILE, label);			\
  if(TARGET_A4)                                         \
     fprintf (FILE, ")");				\
  fprintf(FILE, "\n");                                  \
} while (0)

#define JUMP_ALIGN(LABEL) (arc_size_opt_level < 2 ? 2 : 0)
#define LABEL_ALIGN_AFTER_BARRIER JUMP_ALIGN
/* The desired alignment for the location counter at the beginning
   of a loop.  */
/* On the ARC, align loops to 4 byte boundaries unless doing all-out size
   optimization.  */
#define LOOP_ALIGN JUMP_ALIGN

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
do { if ((LOG) != 0) fprintf (FILE, "\t.align %d\n", 1 << (LOG)); } while (0)

/*  ASM_OUTPUT_ALIGNED_DECL_LOCAL (STREAM, DECL, NAME, SIZE, ALIGNMENT)
    Define this macro when you need to see the variable's decl in order to
    chose what to output. */
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(STREAM, DECL, NAME, SIZE, ALIGNMENT) \
arc_asm_output_aligned_decl_local (STREAM, DECL, NAME, SIZE, ALIGNMENT, 0)

/* Debugging information.  */

/* Generate DBX and DWARF debugging information.  */
#ifdef DBX_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#endif
#define DBX_DEBUGGING_INFO

#ifdef DWARF2_DEBUGGING_INFO
#undef DWARF2_DEBUGGING_INFO
#endif
#define DWARF2_DEBUGGING_INFO

/* Prefer STABS (for now).  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* How to renumber registers for dbx and gdb.  */
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

#define DWARF_FRAME_REGNUM(REG) (REG)

#define DWARF_FRAME_RETURN_COLUMN 	DWARF_FRAME_REGNUM (31)

#define INCOMING_RETURN_ADDR_RTX  gen_rtx_REG (Pmode, 31)

/* Frame info */
/* Force the generation of dwarf .debug_frame sections even if not
   compiling -g.  This guarantees that we can unwind the stack. */

#define DWARF2_FRAME_INFO 1

/* Define this macro to 0 if your target supports DWARF 2 frame unwind
   information, but it does not yet work with exception handling. */
#define DWARF2_UNWIND_INFO 0


/* Turn off splitting of long stabs.  */
#define DBX_CONTIN_LENGTH 0

/* A C statement to output DBX debugging information at the end of
   compilation of the main source file NAME. If you don't define this macro,
   nothing special is output at the end of compilation, which is correct for
   most machines. */
    
/* ashwin */
/* #undef DBX_OUTPUT_MAIN_SOURCE_FILE_END */
/* #define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME)			\ */
/* do {									\ */
/*   text_section ();							\ */
/*   fprintf ((FILE), "\t.stabs \"\",%d,0,0,.Letext\n.Letext:\n", N_SO);	\ */
/* } while (0) */
    
/* Miscellaneous.  */

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/* It's not clear what PIC will look like or whether we want to use -fpic
   for the embedded form currently being talked about.  For now require -fpic
   to get pc relative switch tables.  */
/*#define CASE_VECTOR_PC_RELATIVE 1 */

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND


/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Let the movmem expander handle small block moves.  */
#define MOVE_BY_PIECES_P(LEN, ALIGN)  0
#define CAN_MOVE_BY_PIECES \
  (move_by_pieces_ninsns (SIZE, ALIGN, MOVE_MAX_PIECES + 1) \
   < (unsigned int) MOVE_RATIO)

/* Undo the effects of the movmem pattern presence on STORE_BY_PIECES_P .  */
#define MOVE_RATIO ((unsigned int) (optimize_size ? 3 : 15))

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits. Changed from 1 to 0 for rotate pattern testcases
   (e.g. 20020226-1.c). This change truncates the upper 27 bits of a word
   while rotating a word. Came to notice through a combine phase
   optimization viz. a << (32-b) is equivalent to a << (-b).
*/
#define SHIFT_COUNT_TRUNCATED 0

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */
#define STORE_FLAG_VALUE 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
/* ??? The arc doesn't have full 32 bit pointers, but making this PSImode has
   its own problems (you have to add extendpsisi2 and trucnsipsi2 but how does
   one do it without getting excess code?).  Try to avoid it.  */
#define Pmode SImode

/* A function address in a call instruction.  */
#define FUNCTION_MODE SImode

/* alloca should avoid clobbering the old register save area.  */
/* ??? Not defined in tm.texi.  */
#define SETJMP_VIA_SAVE_AREA

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */
extern struct rtx_def *arc_compare_op0, *arc_compare_op1;

/* Define the function that build the compare insn for scc and bcc.  */
/*extern struct rtx_def *gen_compare_reg ();*/


/* ARC function types.   */
enum arc_function_type {
  ARC_FUNCTION_UNKNOWN, ARC_FUNCTION_NORMAL,
  /* These are interrupt handlers.  The name corresponds to the register
     name that contains the return address.  */
  ARC_FUNCTION_ILINK1, ARC_FUNCTION_ILINK2
};
#define ARC_INTERRUPT_P(TYPE) \
((TYPE) == ARC_FUNCTION_ILINK1 || (TYPE) == ARC_FUNCTION_ILINK2)
/* Compute the type of a function from its DECL.  */
/*enum arc_function_type arc_compute_function_type ();*/

/* Implement `va_start' for varargs and stdarg.  */
#define EXPAND_BUILTIN_VA_START(valist, nextarg) \
  arc_va_start (valist, nextarg)

/* Called by crtstuff.c to make calls to function FUNCTION that are defined in
   SECTION_OP, and then to switch back to text section.  */
#undef CRT_CALL_STATIC_FUNCTION
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC) \
    asm (SECTION_OP "\n\t"				\
	"bl @" USER_LABEL_PREFIX #FUNC "\n"		\
	TEXT_SECTION_ASM_OP);

/* This macro expands to the name of the scratch register r12, used for
 * temporary calculations according to the ABI */
#define ARC_TEMP_SCRATCH_REG "r12"

/* The C++ compiler must use one bit to indicate whether the function
   that will be called through a pointer-to-member-function is
   virtual.  Normally, we assume that the low-order bit of a function
   pointer must always be zero.  Then, by ensuring that the
   vtable_index is odd, we can distinguish which variant of the union
   is in use.  But, on some platforms function pointers can be odd,
   and so this doesn't work.  In that case, we use the low-order bit
   of the `delta' field, and shift the remainder of the `delta' field
   to the left. We need to this for A4 because the address is always 
   shifted and thus can be odd. */
#define TARGET_PTRMEMFUNC_VBIT_LOCATION \
  (TARGET_A4 ? ptrmemfunc_vbit_in_delta : ptrmemfunc_vbit_in_pfn)

#define INSN_SETS_ARE_DELAYED(X)		\
  (GET_CODE (X) == INSN				\
   && GET_CODE (PATTERN (X)) != SEQUENCE	\
   && GET_CODE (PATTERN (X)) != USE		\
   && GET_CODE (PATTERN (X)) != CLOBBER		\
   && get_attr_type (X) == TYPE_CALL)

#define INSN_REFERENCES_ARE_DELAYED(insn) INSN_SETS_ARE_DELAYED (insn)

#define REVERSE_CONDITION(CODE,MODE) \
	(((MODE) == CC_FP_GTmode || (MODE) == CC_FP_GEmode \
	  || (MODE) == CC_FP_UNEQmode || (MODE) == CC_FP_ORDmode \
	  || (MODE) == CC_FPXmode) \
	 ? reverse_condition_maybe_unordered ((CODE)) \
	 : reverse_condition ((CODE)))

#define OUTPUT_ADDR_CONST_EXTRA(FILE, X, FAIL) \
  if (!arc_output_addr_const_extra ((FILE), (X))) \
    goto FAIL;

#if 0
#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2) \
   arc_register_move_cost ((MODE), (CLASS1), (CLASS2))
#endif

#endif /* GCC_ARC_H */
