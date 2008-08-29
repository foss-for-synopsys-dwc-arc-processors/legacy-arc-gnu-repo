/* Subroutines used for code generation on the Argonaut ARC cpu.
   Copyright (C) 1994, 1995, 1997 Free Software Foundation, Inc.

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

#include "config.h"
#include <stdio.h>
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "function.h"
#include "toplev.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "expr.h"
#include "recog.h"
#include "debug.h"
#include "diagnostic.h"
#include "insn-codes.h"
#include "integrate.h"
#include "c-tree.h"
#include "langhooks.h"
#include "optabs.h"
#include "tm-constrs.h"
#include "reload.h" /* For operands_match_p */

/* Which cpu we're compiling for (NULL(=A4), A4, A5, ARC600, ARC700) */
const char *arc_cpu_string;
enum processor_type arc_cpu;
short int mixed_code_enabled = 0; /* By default, GCC won't generate
                                     mixed code for ARCompact ISA */

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */
rtx arc_compare_op0, arc_compare_op1;

/* Name of text, data, and rodata sections used in varasm.c.  */
const char *arc_text_section;
const char *arc_data_section;
const char *arc_rodata_section;

/* Array of valid operand punctuation characters.  */
char arc_punct_chars[256];

/* Variables used by arc_final_prescan_insn to implement conditional
   execution.  */
static int arc_ccfsm_state;
static int arc_ccfsm_current_cc;
static rtx arc_ccfsm_target_insn;
static int arc_ccfsm_target_label;

/* Added for DWARF2 debugging information. cfa_offset is the
   offset of the cfa from the sp. cfa_store_offset is LOC. */
  int cfa_offset;
  int cfa_store_offset;
  int doing_dwarf;

/* local obstack */
static struct obstack arc_local_obstack;

/* The following definition was shifted to arc.h, since #defines from arc.h
   can be freely used in predicates.md */
/* #define PROGRAM_COUNTER_REGNO 63 */

/* The maximum number of insns skipped which will be conditionalised if
   possible.  */
#define MAX_INSNS_SKIPPED 3

/* The values of unspec's first field */
enum { 
  ARC_UNSPEC_PLT = 3, 
  ARC_UNSPEC_GOT, 
  ARC_UNSPEC_GOTOFF
} ;


enum arc_builtins {
  ARC_BUILTIN_NOP        =    2,
  ARC_BUILTIN_NORM       =    3,
  ARC_BUILTIN_NORMW      =    4,
  ARC_BUILTIN_SWAP       =    5,
  ARC_BUILTIN_BRK        =    6,
  ARC_BUILTIN_DIVAW      =    7,
  ARC_BUILTIN_EX         =    8,
  ARC_BUILTIN_MUL64      =    9,
  ARC_BUILTIN_MULU64     =   10,
  ARC_BUILTIN_RTIE       =   11,
  ARC_BUILTIN_SYNC       =   12,
  ARC_BUILTIN_CORE_READ  =   13,
  ARC_BUILTIN_CORE_WRITE =   14,
  ARC_BUILTIN_FLAG       =   15,
  ARC_BUILTIN_LR         =   16,
  ARC_BUILTIN_SR         =   17,
  ARC_BUILTIN_SLEEP      =   18,
  ARC_BUILTIN_SWI        =   19,
  ARC_BUILTIN_TRAP_S     =   20,
  ARC_BUILTIN_UNIMP_S    =   21,

  /* Sentinel to mark start of simd builtins */
  ARC_SIMD_BUILTIN_BEGIN      = 1000,

  ARC_SIMD_BUILTIN_VADDAW     = 1001,
  ARC_SIMD_BUILTIN_VADDW      = 1002,
  ARC_SIMD_BUILTIN_VAVB       = 1003,
  ARC_SIMD_BUILTIN_VAVRB      = 1004,
  ARC_SIMD_BUILTIN_VDIFAW     = 1005,
  ARC_SIMD_BUILTIN_VDIFW      = 1006,
  ARC_SIMD_BUILTIN_VMAXAW     = 1007,
  ARC_SIMD_BUILTIN_VMAXW      = 1008,
  ARC_SIMD_BUILTIN_VMINAW     = 1009,
  ARC_SIMD_BUILTIN_VMINW      = 1010,
  ARC_SIMD_BUILTIN_VMULAW     = 1011,
  ARC_SIMD_BUILTIN_VMULFAW    = 1012,
  ARC_SIMD_BUILTIN_VMULFW     = 1013,
  ARC_SIMD_BUILTIN_VMULW      = 1014,
  ARC_SIMD_BUILTIN_VSUBAW     = 1015,
  ARC_SIMD_BUILTIN_VSUBW      = 1016,
  ARC_SIMD_BUILTIN_VSUMMW     = 1017,
  ARC_SIMD_BUILTIN_VAND       = 1018,
  ARC_SIMD_BUILTIN_VANDAW     = 1019,
  ARC_SIMD_BUILTIN_VBIC       = 1020,
  ARC_SIMD_BUILTIN_VBICAW     = 1021,
  ARC_SIMD_BUILTIN_VOR        = 1022,
  ARC_SIMD_BUILTIN_VXOR       = 1023,
  ARC_SIMD_BUILTIN_VXORAW     = 1024,
  ARC_SIMD_BUILTIN_VEQW       = 1025,
  ARC_SIMD_BUILTIN_VLEW       = 1026,
  ARC_SIMD_BUILTIN_VLTW       = 1027,
  ARC_SIMD_BUILTIN_VNEW       = 1028,
  ARC_SIMD_BUILTIN_VMR1AW     = 1029,
  ARC_SIMD_BUILTIN_VMR1W      = 1030,
  ARC_SIMD_BUILTIN_VMR2AW     = 1031,
  ARC_SIMD_BUILTIN_VMR2W      = 1032,
  ARC_SIMD_BUILTIN_VMR3AW     = 1033,
  ARC_SIMD_BUILTIN_VMR3W      = 1034,
  ARC_SIMD_BUILTIN_VMR4AW     = 1035,
  ARC_SIMD_BUILTIN_VMR4W      = 1036,
  ARC_SIMD_BUILTIN_VMR5AW     = 1037,
  ARC_SIMD_BUILTIN_VMR5W      = 1038,
  ARC_SIMD_BUILTIN_VMR6AW     = 1039,
  ARC_SIMD_BUILTIN_VMR6W      = 1040,
  ARC_SIMD_BUILTIN_VMR7AW     = 1041,
  ARC_SIMD_BUILTIN_VMR7W      = 1042,
  ARC_SIMD_BUILTIN_VMRB       = 1043,
  ARC_SIMD_BUILTIN_VH264F     = 1044,
  ARC_SIMD_BUILTIN_VH264FT    = 1045,
  ARC_SIMD_BUILTIN_VH264FW    = 1046,
  ARC_SIMD_BUILTIN_VVC1F      = 1047,
  ARC_SIMD_BUILTIN_VVC1FT     = 1048,

  /* Va, Vb, rlimm instructions */
  ARC_SIMD_BUILTIN_VBADDW     = 1050,
  ARC_SIMD_BUILTIN_VBMAXW     = 1051,
  ARC_SIMD_BUILTIN_VBMINW     = 1052,
  ARC_SIMD_BUILTIN_VBMULAW    = 1053,
  ARC_SIMD_BUILTIN_VBMULFW    = 1054,
  ARC_SIMD_BUILTIN_VBMULW     = 1055,
  ARC_SIMD_BUILTIN_VBRSUBW    = 1056,
  ARC_SIMD_BUILTIN_VBSUBW     = 1057,

  /* Va, Vb, Ic instructions */
  ARC_SIMD_BUILTIN_VASRW      = 1060,
  ARC_SIMD_BUILTIN_VSR8       = 1061,
  ARC_SIMD_BUILTIN_VSR8AW     = 1062,

  /* Va, Vb, u6 instructions */
  ARC_SIMD_BUILTIN_VASRRWi    = 1065,
  ARC_SIMD_BUILTIN_VASRSRWi   = 1066,
  ARC_SIMD_BUILTIN_VASRWi     = 1067,
  ARC_SIMD_BUILTIN_VASRPWBi   = 1068,
  ARC_SIMD_BUILTIN_VASRRPWBi  = 1069,
  ARC_SIMD_BUILTIN_VSR8AWi    = 1070,
  ARC_SIMD_BUILTIN_VSR8i      = 1071,

  /* Va, Vb, u8 (simm) instructions*/
  ARC_SIMD_BUILTIN_VMVAW      = 1075,
  ARC_SIMD_BUILTIN_VMVW       = 1076,
  ARC_SIMD_BUILTIN_VMVZW      = 1077,
  ARC_SIMD_BUILTIN_VD6TAPF    = 1078,

  /* Va, rlimm, u8 (simm) instructions*/
  ARC_SIMD_BUILTIN_VMOVAW     = 1080,
  ARC_SIMD_BUILTIN_VMOVW      = 1081,
  ARC_SIMD_BUILTIN_VMOVZW     = 1082,

  /* Va, Vb instructions */
  ARC_SIMD_BUILTIN_VABSAW     = 1085,
  ARC_SIMD_BUILTIN_VABSW      = 1086,
  ARC_SIMD_BUILTIN_VADDSUW    = 1087,
  ARC_SIMD_BUILTIN_VSIGNW     = 1088,
  ARC_SIMD_BUILTIN_VEXCH1     = 1089,
  ARC_SIMD_BUILTIN_VEXCH2     = 1090,
  ARC_SIMD_BUILTIN_VEXCH4     = 1091,
  ARC_SIMD_BUILTIN_VUPBAW     = 1092,
  ARC_SIMD_BUILTIN_VUPBW      = 1093,
  ARC_SIMD_BUILTIN_VUPSBAW    = 1094,
  ARC_SIMD_BUILTIN_VUPSBW     = 1095,

  ARC_SIMD_BUILTIN_VDIRUN     = 1100,
  ARC_SIMD_BUILTIN_VDORUN     = 1101,
  ARC_SIMD_BUILTIN_VDIWR      = 1102,
  ARC_SIMD_BUILTIN_VDOWR      = 1103,

  ARC_SIMD_BUILTIN_VREC       = 1105,
  ARC_SIMD_BUILTIN_VRUN       = 1106,
  ARC_SIMD_BUILTIN_VRECRUN    = 1107,
  ARC_SIMD_BUILTIN_VENDREC    = 1108,

  ARC_SIMD_BUILTIN_VLD32WH    = 1110,
  ARC_SIMD_BUILTIN_VLD32WL    = 1111,
  ARC_SIMD_BUILTIN_VLD64      = 1112,
  ARC_SIMD_BUILTIN_VLD32      = 1113,
  ARC_SIMD_BUILTIN_VLD64W     = 1114,
  ARC_SIMD_BUILTIN_VLD128     = 1115,
  ARC_SIMD_BUILTIN_VST128     = 1116,
  ARC_SIMD_BUILTIN_VST64      = 1117,

  ARC_SIMD_BUILTIN_VST16_N    = 1120,
  ARC_SIMD_BUILTIN_VST32_N    = 1121,

  ARC_SIMD_BUILTIN_VINTI      = 1201,

  ARC_SIMD_BUILTIN_END
};

/* A nop is needed between a 4 byte insn that sets the condition codes and
   a branch that uses them (the same isn't true for an 8 byte insn that sets
   the condition codes).  Set by arc_final_prescan_insn.  Used by
   arc_print_operand.  */

static int get_arc_condition_code (rtx);
/* Initialized arc_attribute_table to NULL since arc doesnot have any
   machine specific supported attributes. */
const struct attribute_spec arc_attribute_table[] =
{
 /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  /* Function calls made to this symbol must be done indirectly, because
     it may lie outside of the 21/25 bit addressing range of a normal function
     call.  */
  { "long_call",    0, 0, false, true,  true,  NULL },
  /* Whereas these functions are always known to reside within the 21/25 bit
     addressing range.  */
  { "short_call",   0, 0, false, true,  true,  NULL },
  { NULL, 0, 0, false, false, false, NULL }
};
static bool arc_assemble_integer (rtx, unsigned int, int);
static void arc_output_function_prologue (FILE *, HOST_WIDE_INT);
static void arc_output_function_epilogue (FILE *, HOST_WIDE_INT);
static int arc_comp_type_attributes (tree, tree);
static void arc_file_start (void);
static void arc_asm_file_start (FILE *)  ATTRIBUTE_UNUSED;
static void arc_asm_file_end (void);
static void arc_internal_label (FILE *, const char *, unsigned long);
static void arc_output_mi_thunk (FILE *, tree, HOST_WIDE_INT, HOST_WIDE_INT,
				 tree);
static bool arc_rtx_costs (rtx, int, int, int *);
static int arc_address_cost (rtx);
static void arc_encode_section_info (tree decl, rtx rtl, int first);
static const char *arc_strip_name_encoding (const char *name);
static bool arc_cannot_force_const_mem (rtx);

static void arc_init_builtins (void);
static rtx arc_expand_builtin (tree, rtx, rtx, enum machine_mode, int);

static int branch_dest (rtx);
static void arc_encode_symbol (tree, const char);

static void  arc_output_pic_addr_const (FILE *,  rtx, int);
int symbolic_reference_mentioned_p (rtx);
void arc_assemble_name (FILE *, const char*);
int arc_raw_symbolic_reference_mentioned_p (rtx);
int arc_legitimate_pic_addr_p (rtx) ATTRIBUTE_UNUSED;
void emit_pic_move (rtx *, enum machine_mode) ATTRIBUTE_UNUSED;
bool arc_legitimate_pic_operand_p (rtx);
bool arc_legitimate_constant_p (rtx);
static bool arc_function_ok_for_sibcall (tree, tree);
const char * output_shift (rtx *);
static void arc_reorg (void);
static bool arc_in_small_data_p (tree);

static void arc_init_reg_tables (void);
static bool arc_pass_by_reference (CUMULATIVE_ARGS *, enum machine_mode, tree , bool);
static int arc_arg_partial_bytes (CUMULATIVE_ARGS *, enum machine_mode,
				  tree, bool);

static void arc_init_simd_builtins (void);
static bool arc_vector_mode_supported_p (enum machine_mode);

static const char *arc_invalid_within_doloop (rtx);

/* Implements target hook vector_mode_supported_p.  */
static bool
arc_vector_mode_supported_p (enum machine_mode mode)
{
  if (!TARGET_SIMD_SET)
    return false;

  if ((mode == V4SImode)
      || (mode == V8HImode))
    return true;

  return false;
}

/* to be defined for interrupt attribute addition */
/*static tree arc_handle_interrupt_attribute (tree *, tree, tree, int, bool *);*/


static int last_insn_set_cc_p; /* ??? Use was deleted. Check if this variable can go.  */
static int current_insn_set_cc_p;
static bool arc_preserve_reload_p (rtx in);
static rtx arc_delegitimize_address (rtx);

/* initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER arc_assemble_integer
#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE arc_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE arc_output_function_epilogue
#undef  TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES arc_comp_type_attributes
#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START arc_file_start
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END arc_asm_file_end
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE arc_attribute_table
#undef TARGET_ASM_INTERNAL_LABEL
#define TARGET_ASM_INTERNAL_LABEL arc_internal_label
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS arc_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST arc_address_cost

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO arc_encode_section_info

#undef TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING arc_strip_name_encoding

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM arc_cannot_force_const_mem

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  arc_init_builtins

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN arc_expand_builtin

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK arc_output_mi_thunk

#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_tree_hwi_hwi_tree_true

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL arc_function_ok_for_sibcall

#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG arc_reorg

#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P arc_in_small_data_p

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS arc_setup_incoming_varargs

#undef TARGET_PROMOTE_FUNCTION_ARGS
#define TARGET_PROMOTE_FUNCTION_ARGS hook_bool_tree_true

#undef TARGET_PROMOTE_FUNCTION_RETURN
#define TARGET_PROMOTE_FUNCTION_RETURN hook_bool_tree_true

#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_tree_true

/* ashwin : don't know if the following is to be defined */
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE arc_pass_by_reference

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES arc_arg_partial_bytes

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE arc_function_value

#ifdef USE_UCLIBC
#define DEFAULT_NO_SDATA MASK_NO_SDATA_SET
#else
#define DEFAULT_NO_SDATA 0
#endif
#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS  (MASK_VOLATILE_CACHE_SET|DEFAULT_NO_SDATA)

#undef  TARGET_SCHED_ADJUST_PRIORITY 
#define TARGET_SCHED_ADJUST_PRIORITY arc_sched_adjust_priority

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P arc_vector_mode_supported_p

#undef TARGET_INVALID_WITHIN_DOLOOP
#define TARGET_INVALID_WITHIN_DOLOOP arc_invalid_within_doloop

#undef TARGET_PRESERVE_RELOAD_P
#define TARGET_PRESERVE_RELOAD_P arc_preserve_reload_p

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS arc_delegitimize_address

/* Try to keep the (mov:DF _, reg) as early as possible so 
   that the d<add/sub/mul>h-lr insns appear together and can
   use the peephole2 pattern
*/
static int
arc_sched_adjust_priority (rtx insn ATTRIBUTE_UNUSED, int priority)
{
  rtx set = single_set (insn);
  if (set 
      && GET_MODE (SET_SRC(set)) == DFmode
      && GET_CODE (SET_SRC(set)) == REG)
    {
      /* Incrementing priority by 20 (empirically derived). */
      return priority + 20;
    }

  return priority;
}

struct gcc_target targetm = TARGET_INITIALIZER;

/* Called by OVERRIDE_OPTIONS to initialize various things.  */
void arc_init (void)
{
  char *tmp;
  int target_found = 0;
  enum attr_tune tune_dflt = TUNE_NONE;

  if (TARGET_A4)
    {
      arc_cpu_string = "A4";
      arc_cpu = PROCESSOR_A4;
      target_found = 1;
    }
  else if (TARGET_A5)
    {
      arc_cpu_string = "A5";
      arc_cpu = PROCESSOR_A5;
      target_found = 1;
    }
  else if (TARGET_ARC600)
    {
      arc_cpu_string = "ARC600";
      arc_cpu = PROCESSOR_ARC600;
      tune_dflt = TUNE_ARC600;
      target_found = 1;
    }
  else if (TARGET_ARC700)
    {
      arc_cpu_string = "ARC700";
      arc_cpu = PROCESSOR_ARC700;
      tune_dflt = TUNE_ARC700_4_2_STD;
      target_found = 1;
    }
  if (arc_tune == TUNE_NONE)
    arc_tune = tune_dflt;
  if (arc_multcost < 0)
    switch (arc_tune)
      {
      case TUNE_ARC700_4_2_STD:  /* ??? Should that be COSTS_N_INSNS (4); ?  */
      case TUNE_ARC700_4_2_XMAC: /* ??? Should that be COSTS_N_INSNS (3); ?  */
	arc_multcost = COSTS_N_INSNS (6);
	break;
      default:
	arc_multcost = COSTS_N_INSNS (30);
	break;
      }
  if (TARGET_MIXED_CODE_SET)
    {
      /* -mmixed-code can not be used with the option -mA4. */
      if (TARGET_A4)
        {
          error ("-mmixed-code can't be used with the option -mA4");
        }

      /* If -mmixed-code option is given but target option is *not* given,
         then ARC700 will be automatically selected */
      if (!target_found) 
        {
          target_flags |= MASK_ARC700;
          arc_cpu_string = "ARC700";
          arc_cpu = PROCESSOR_ARC700;
          target_found = 1;
        }

      mixed_code_enabled = 1;
    }
  
  /* If none of the target option (-mA4,-mA5,-mARC600,-mARC700) is given,
     select -mA5 as default. */
  if (!target_found)
    {
#ifndef USE_UCLIBC
      target_flags |= MASK_A5;
      arc_cpu_string = "A5";
      arc_cpu = PROCESSOR_A5;
#else
      target_flags |= MASK_ARC700;
      arc_cpu_string = "ARC700";
      arc_cpu = PROCESSOR_ARC700;

#endif
    }

  /* Support mul64 generation only for A4, A5 and ARC600 */
  if (TARGET_MUL64_SET && TARGET_ARC700)
      error ("-mmul64 not supported for ARC700");

  /* MPY instructions valid only for ARC700 */
  if (TARGET_NOMPY_SET && !TARGET_ARC700)
      error ("-mno-mpy supported only for ARC700");

  /* Sanity checks for usage of the FPX switches */
  /* FPX-1. No fast and compact together */
  if ((TARGET_DPFP_FAST_SET && TARGET_DPFP_COMPACT_SET)
      || (TARGET_SPFP_FAST_SET && TARGET_SPFP_COMPACT_SET))
    error ("FPX fast and compact options cannot be specified together");

  /* FPX-2. No fast-spfp for arc600 */
  if (TARGET_SPFP_FAST_SET && TARGET_ARC600)
    error ("-mspfp_fast not available on ARC600");

  /* FPX-3. No FPX extensions on pre-ARC600 cores */
  if ((TARGET_DPFP || TARGET_SPFP) 
      && !(TARGET_ARC600 || TARGET_ARC700))
    error ("FPX extensions not available on pre-ARC600 cores");

  /* Warn for unimplemented PIC in pre-ARC700 cores, and disable flag_pic */
  if (flag_pic && !TARGET_ARC700)
    {
      warning (DK_WARNING, "PIC is not supported for %s. Generating non-PIC code only..", arc_cpu_string);
      flag_pic = 0;
    }

  /* Set the pseudo-ops for the various standard sections.  */
  arc_text_section = tmp = xmalloc (strlen (arc_text_string) + sizeof (ARC_SECTION_FORMAT) + 1);
  sprintf (tmp, ARC_SECTION_FORMAT, arc_text_string);
  arc_data_section = tmp = xmalloc (strlen (arc_data_string) + sizeof (ARC_SECTION_FORMAT) + 1);
  sprintf (tmp, ARC_SECTION_FORMAT, arc_data_string);
  arc_rodata_section = tmp =xmalloc (strlen (arc_rodata_string) + sizeof (ARC_SECTION_FORMAT) + 1);
  sprintf (tmp, ARC_SECTION_FORMAT, arc_rodata_string);

  arc_init_reg_tables ();

  /* Initialize array for PRINT_OPERAND_PUNCT_VALID_P.  */
  memset (arc_punct_chars, 0, sizeof (arc_punct_chars));
  arc_punct_chars['#'] = 1;
  arc_punct_chars['*'] = 1;
  arc_punct_chars['?'] = 1;
  arc_punct_chars['!'] = 1;
  arc_punct_chars['^'] = 1;
  gcc_obstack_init (&arc_local_obstack);
}

/* The condition codes of the ARC, and the inverse function.  */
static const char *arc_condition_codes[] =
{
  /*"al", 0, "eq", "ne", "p", "n", "c", "nc", "v", "nv",*/
  "al", 0, "eq", "ne", "p", "n", "lo", "hs", "v", "nv",
  "gt", "le", "ge", "lt", "hi", "ls", "pnz", 0
};

#define ARC_INVERSE_CONDITION_CODE(X)  ((X) ^ 1)

/* Returns the index of the ARC condition code string in
   `arc_condition_codes'.  COMPARISON should be an rtx like
   `(eq (...) (...))'.  */

static int
get_arc_condition_code (rtx comparison)
{
  switch (GET_MODE (XEXP (comparison, 0)))
    {
    case CCmode:
    case SImode: /* For BRcc.  */
      switch (GET_CODE (comparison))
	{
	case EQ : return 2;
	case NE : return 3;
	case GT : return 10;
	case LE : return 11;
	case GE : return 12;
	case LT : return 13;
	case GTU : return 14;
	case LEU : return 15;
	case LTU : return 6;
	case GEU : return 7;
	default : gcc_unreachable ();
	}
    case CC_ZNmode:
      switch (GET_CODE (comparison))
	{
	case EQ : return 2;
	case NE : return 3;
	case GE: return 4; /* p */
	case LT: return 5; /* n */
	case GT : return 16; /* pnz */
	default : gcc_unreachable ();
	}
    case CC_Zmode:
      switch (GET_CODE (comparison))
	{
	case EQ : return 2;
	case NE : return 3;
	default : gcc_unreachable ();
	}
    case CC_Cmode:
      switch (GET_CODE (comparison))
	{
	case LTU : return 6;
	case GEU : return 7;
	default : gcc_unreachable ();
	}
    case CC_FP_GTmode:
      if (TARGET_SPFP)
	switch (GET_CODE (comparison))
	  {
	  case GT  : return 5; /* n */
	  case UNLE: return 4; /* p */
	  default : gcc_unreachable ();
	}
      else
	switch (GET_CODE (comparison))
	  {
	  case GT   : return 14;
	  case UNLE : return 15;
	  default : gcc_unreachable ();
	}
    case CC_FP_GEmode:
      /* Same for FPX and non-FPX.  */
      switch (GET_CODE (comparison))
	{
	case GE   : return 7;
	case UNLT : return 6;
	default : gcc_unreachable ();
	}
    case CC_FP_UNEQmode:
      switch (GET_CODE (comparison))
	{
	case UNEQ : return 2;
	case LTGT : return 3;
	default : gcc_unreachable ();
	}
    case CC_FP_ORDmode:
      switch (GET_CODE (comparison))
	{
	case UNORDERED : return 6;
	case ORDERED   : return 7;
	default : gcc_unreachable ();
	}
    case CC_FPXmode:
      switch (GET_CODE (comparison))
	{
	case EQ        : return 2;
	case NE        : return 3;
	case UNORDERED : return 6;
	case ORDERED   : return 7;
	case LTGT      : return 14;
	case UNEQ      : return 15;
	default : gcc_unreachable ();
	}
    default : gcc_unreachable ();
    }
  /*NOTREACHED*/
  return (42);
}

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */

enum machine_mode
arc_select_cc_mode (enum rtx_code op,	
		    rtx x ATTRIBUTE_UNUSED,
		    rtx y ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = GET_MODE (x);

  /* For an operation that sets the condition codes as a side-effect, the
     C and V flags is not set as for cmp, so we can only use comparisons where
     this doesn't matter.  (For LT and GE we can use "mi" and "pl"
     instead.)  */
  /* ??? We could use "pnz" for greater than zero, however, we could then
     get into trouble because the comparison could not be reversed.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && y == const0_rtx
      && (op == EQ || op == NE
	  || ((op == LT || op == GE) && GET_MODE_SIZE (GET_MODE (x) <= 4))))
    return CC_ZNmode;

  /* Check if this is a test suitable for bxor.f  */
  if (mode == SImode && (op == EQ || op == NE) && CONST_INT_P (y)
      && ((INTVAL (y) - 1) & INTVAL (y)) == 0
      && INTVAL (y))
    return CC_Zmode;

  if (GET_MODE (x) == SImode && (op == LTU || op == GEU)
      && GET_CODE (x) == PLUS
      && (rtx_equal_p (XEXP (x, 0), y) || rtx_equal_p (XEXP (x, 1), y)))
    return CC_Cmode;

  if ((mode == SFmode && TARGET_SPFP) || (mode == DFmode && TARGET_DPFP))
    switch (op)
      {
      case EQ: case NE: case UNEQ: case LTGT: case ORDERED: case UNORDERED:
	return CC_FPXmode;
      case LT: case UNGE: case GT: case UNLE:
	return CC_FP_GTmode;
      case LE: case UNGT: case GE: case UNLT:
	return CC_FP_GEmode;
      default: gcc_unreachable ();
      }
  else if (GET_MODE_CLASS (mode) == MODE_FLOAT && TARGET_ARC700)
    switch (op)
      {
      case EQ: case NE: return CC_Zmode;
      case LT: case UNGE:
      case GT: case UNLE: return CC_FP_GTmode;
      case LE: case UNGT:
      case GE: case UNLT: return CC_FP_GEmode;
      case UNEQ: case LTGT: return CC_FP_UNEQmode;
      case ORDERED: case UNORDERED: return CC_FP_ORDmode;
      default: gcc_unreachable ();
      }

  return CCmode;
}

/* Vectors to keep interesting information about registers where it can easily
   be got.  We use to use the actual mode value as the bit number, but there
   is (or may be) more than 32 modes now.  Instead we use two tables: one
   indexed by hard register number, and one indexed by mode.  */

/* The purpose of arc_mode_class is to shrink the range of modes so that
   they all fit (as bit numbers) in a 32 bit word (again).  Each real mode is
   mapped into one arc_mode_class mode.  */

enum arc_mode_class {
  C_MODE,
  S_MODE, D_MODE, T_MODE, O_MODE,
  SF_MODE, DF_MODE, TF_MODE, OF_MODE,
  V_MODE
};

/* Modes for condition codes.  */
#define C_MODES (1 << (int) C_MODE)

/* Modes for single-word and smaller quantities.  */
#define S_MODES ((1 << (int) S_MODE) | (1 << (int) SF_MODE))

/* Modes for double-word and smaller quantities.  */
#define D_MODES (S_MODES | (1 << (int) D_MODE) | (1 << DF_MODE))

/* Mode for 8-byte DF values only */
#define DF_MODES (1 << DF_MODE)

/* Modes for quad-word and smaller quantities.  */
#define T_MODES (D_MODES | (1 << (int) T_MODE) | (1 << (int) TF_MODE))

/* Modes for 128-bit vectors.  */
#define V_MODES (1 << (int) V_MODE)

/* Value is 1 if register/mode pair is acceptable on arc.  */

unsigned int arc_hard_regno_mode_ok[] = {
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES,
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES,
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, D_MODES,
  D_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES,

  /* ??? Leave these as S_MODES for now.  */
  S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES,
  DF_MODES, 0, DF_MODES, 0, S_MODES, S_MODES, S_MODES, S_MODES,
  S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES,
  S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, C_MODES, S_MODES,

  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,

  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,

  S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES,
  S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES
};

unsigned int arc_mode_class [NUM_MACHINE_MODES];

enum reg_class arc_regno_reg_class[FIRST_PSEUDO_REGISTER];

static void
arc_init_reg_tables (void)
{
  int i;

  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      switch (GET_MODE_CLASS (i))
	{
	case MODE_INT:
	case MODE_PARTIAL_INT:
	case MODE_COMPLEX_INT:
	  if (GET_MODE_SIZE (i) <= 4)
	    arc_mode_class[i] = 1 << (int) S_MODE;
	  else if (GET_MODE_SIZE (i) == 8)
	    arc_mode_class[i] = 1 << (int) D_MODE;
	  else if (GET_MODE_SIZE (i) == 16)
	    arc_mode_class[i] = 1 << (int) T_MODE;
	  else if (GET_MODE_SIZE (i) == 32)
	    arc_mode_class[i] = 1 << (int) O_MODE;
	  else 
	    arc_mode_class[i] = 0;
	  break;
	case MODE_FLOAT:
	case MODE_COMPLEX_FLOAT:
	  if (GET_MODE_SIZE (i) <= 4)
	    arc_mode_class[i] = 1 << (int) SF_MODE;
	  else if (GET_MODE_SIZE (i) == 8)
	    arc_mode_class[i] = 1 << (int) DF_MODE;
	  else if (GET_MODE_SIZE (i) == 16)
	    arc_mode_class[i] = 1 << (int) TF_MODE;
	  else if (GET_MODE_SIZE (i) == 32)
	    arc_mode_class[i] = 1 << (int) OF_MODE;
	  else 
	    arc_mode_class[i] = 0;
	  break;
	case MODE_VECTOR_INT:
	  arc_mode_class [i] = (1<< (int) V_MODE);
	  break;
	case MODE_CC:
	default:
	  /* mode_class hasn't been initialized yet for EXTRA_CC_MODES, so
	     we must explicitly check for them here.  */
	  if (i == (int) CCmode || i == (int) CC_ZNmode || i == (int) CC_Zmode
	      || i == (int) CC_Cmode
	      || i == CC_FP_GTmode || i == CC_FP_GEmode || i == CC_FP_ORDmode)
	    arc_mode_class[i] = 1 << (int) C_MODE;
	  else
	    arc_mode_class[i] = 0;
	  break;
	}
    }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (i < 29)
        {
          if (mixed_code_enabled && ((i <= 3) || ((i >= 12) && (i <= 15))))
            arc_regno_reg_class[i] = ARCOMPACT16_REGS;
          else
            arc_regno_reg_class[i] = GENERAL_REGS;
        }
      else
        {
          arc_regno_reg_class[i] = NO_REGS;
        } /* if */
    }

    /* ARCOMPACT16_REGS is empty, if mixed code has not bee activated */
      if (!mixed_code_enabled)
      { 
	CLEAR_HARD_REG_SET(reg_class_contents [ARCOMPACT16_REGS]);
	CLEAR_HARD_REG_SET(reg_class_contents [AC16_BASE_REGS]);
      }

    gcc_assert (FIRST_PSEUDO_REGISTER >= 144);

    /* Handle Special Registers */
    arc_regno_reg_class[29] = LINK_REGS; /* ilink1 register */
    arc_regno_reg_class[30] = LINK_REGS; /* ilink2 register */
    arc_regno_reg_class[31] = LINK_REGS; /* blink register */
    arc_regno_reg_class[60] = LPCOUNT_REG;
    arc_regno_reg_class[61] = NO_REGS;      /* CC_REG: must be NO_REGS */
    arc_regno_reg_class[62] = GENERAL_REGS;

    if (TARGET_DPFP)
      {
	arc_regno_reg_class[40] = DOUBLE_REGS;
	arc_regno_reg_class[41] = DOUBLE_REGS;
	arc_regno_reg_class[42] = DOUBLE_REGS;
	arc_regno_reg_class[43] = DOUBLE_REGS;
      }
    else
      {
	/* Disable all DOUBLE_REGISTER settings, 
	   if not generating DPFP code */
	arc_regno_reg_class[40] = ALL_REGS;
	arc_regno_reg_class[41] = ALL_REGS;
	arc_regno_reg_class[42] = ALL_REGS;
	arc_regno_reg_class[43] = ALL_REGS;

	arc_hard_regno_mode_ok[40] = 0;
	arc_hard_regno_mode_ok[42] = 0;

	CLEAR_HARD_REG_SET(reg_class_contents [DOUBLE_REGS]);
      }

    if (TARGET_SIMD_SET)
      {
	gcc_assert (ARC_FIRST_SIMD_VR_REG == 64);
	gcc_assert (ARC_LAST_SIMD_VR_REG  == 127);

	for (i = ARC_FIRST_SIMD_VR_REG; i <= ARC_LAST_SIMD_VR_REG; i++)
	  arc_regno_reg_class [i] =  SIMD_VR_REGS;

	gcc_assert (ARC_FIRST_SIMD_DMA_CONFIG_REG == 128);
	gcc_assert (ARC_FIRST_SIMD_DMA_CONFIG_IN_REG == 128);
	gcc_assert (ARC_FIRST_SIMD_DMA_CONFIG_OUT_REG == 136);
	gcc_assert (ARC_LAST_SIMD_DMA_CONFIG_REG  == 143);

	for (i = ARC_FIRST_SIMD_DMA_CONFIG_REG; i <= ARC_LAST_SIMD_DMA_CONFIG_REG; i++)
	  arc_regno_reg_class [i] =  SIMD_DMA_CONFIG_REGS;
      }

    /* pc : r63 */
    arc_regno_reg_class[PROGRAM_COUNTER_REGNO] = GENERAL_REGS; 
}

/* ARC specific attribute support.

   The ARC has these attributes:
   interrupt - for interrupt functions
*/

/* Return nonzero if IDENTIFIER is a valid decl attribute.  */

int
arc_valid_machine_decl_attribute (tree type ATTRIBUTE_UNUSED,
				  tree attributes ATTRIBUTE_UNUSED,
				  tree identifier ATTRIBUTE_UNUSED,
				  tree args ATTRIBUTE_UNUSED)
{
  if (identifier == get_identifier ("__nterrupt__")
      && list_length (args) == 1
      && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
    {
      tree value = TREE_VALUE (args);

      if (!strcmp (TREE_STRING_POINTER (value), "ilink1")
	   || !strcmp (TREE_STRING_POINTER (value), "ilink2"))
	return 1;
    }
  return 0;
}

/* Return zero if TYPE1 and TYPE are incompatible, one if they are compatible,
   and two if they are nearly compatible (which causes a warning to be
   generated).  */

static int
arc_comp_type_attributes (tree type1,
			  tree type2)
{
  int l1, l2, s1, s2;
  
  /* Check for mismatch of non-default calling convention.  */
  if (TREE_CODE (type1) != FUNCTION_TYPE)
    return 1;

  /* Check for mismatched call attributes.  */
  l1 = lookup_attribute ("long_call", TYPE_ATTRIBUTES (type1)) != NULL;
  l2 = lookup_attribute ("long_call", TYPE_ATTRIBUTES (type2)) != NULL;
  s1 = lookup_attribute ("short_call", TYPE_ATTRIBUTES (type1)) != NULL;
  s2 = lookup_attribute ("short_call", TYPE_ATTRIBUTES (type2)) != NULL;

  /* Only bother to check if an attribute is defined.  */
  if (l1 | l2 | s1 | s2)
    {
      /* If one type has an attribute, the other must have the same attribute.  */
      if ((l1 != l2) || (s1 != s2))
	return 0;

      /* Disallow mixed attributes.  */
      if ((l1 & s2) || (l2 & s1))
	return 0;
    }
  

  return 1;
}

/* Set the default attributes for TYPE.  */

void
arc_set_default_type_attributes (tree type ATTRIBUTE_UNUSED)
{
  gcc_unreachable();
}

/* Misc. utilities.  */

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for the cc reg in the proper mode.  */

rtx
gen_compare_reg (enum rtx_code code, enum machine_mode omode)
{
  rtx x = arc_compare_op0, y = arc_compare_op1;
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  enum machine_mode cmode = GET_MODE (x);
  rtx cc_reg;

  cc_reg = gen_rtx_REG (mode, 61);

  if ((cmode == SFmode && TARGET_SPFP) || (cmode == DFmode && TARGET_DPFP))
    {
      switch (code)
	{
	case NE: case EQ: case LT: case UNGE: case LE: case UNGT:
	case UNEQ: case LTGT: case ORDERED: case UNORDERED:
	  break;
	case GT: case UNLE: case GE: case UNLT:
	  code = swap_condition (code);
	  x = arc_compare_op1;
	  y = arc_compare_op0;
	  break;
	default:
	  gcc_unreachable ();
	}
      emit_insn ((cmode == SFmode ? gen_cmpsfpx_raw : gen_cmpdfpx_raw) (x, y));
      if (mode != CC_FPXmode)
	emit_insn (gen_rtx_SET (VOIDmode, cc_reg,
				gen_rtx_COMPARE (mode,
						 gen_rtx_REG (CC_FPXmode, 61),
						 const0_rtx)));
    }
  else if (GET_MODE_CLASS (cmode) == MODE_FLOAT && TARGET_ARC700)
    {
      rtx op0 = gen_rtx_REG (cmode, 0);
      rtx op1 = gen_rtx_REG (cmode, GET_MODE_SIZE (cmode) / UNITS_PER_WORD);

      switch (code)
	{
	case NE: case EQ: case GT: case UNLE: case GE: case UNLT:
	case UNEQ: case LTGT: case ORDERED: case UNORDERED:
	  break;
	case LT: case UNGE: case LE: case UNGT:
	  code = swap_condition (code);
	  x = arc_compare_op1;
	  y = arc_compare_op0;
	  break;
	default:
	  gcc_unreachable ();
	}
      if (currently_expanding_to_rtl)
	{
	  emit_move_insn (op0, x);
	  emit_move_insn (op1, y);
	}
      else
	{
	  gcc_assert (rtx_equal_p (op0, x));
	  gcc_assert (rtx_equal_p (op1, y));
	}
      emit_insn (gen_cmp_float (cc_reg, gen_rtx_COMPARE (mode, op0, op1)));
    }
  else
    emit_insn (gen_rtx_SET (omode, cc_reg,
			    gen_rtx_COMPARE (mode, x, y)));
  return gen_rtx_fmt_ee (code, omode, cc_reg, const0_rtx);
}

/* Return 1 if VALUE, a const_double, will fit in a limm (4 byte number).
   We assume the value can be either signed or unsigned.  */

int
arc_double_limm_p (rtx value)
{
  HOST_WIDE_INT low, high;

  gcc_assert (GET_CODE (value) == CONST_DOUBLE);

  if(TARGET_DPFP) 
    return 1;

  low = CONST_DOUBLE_LOW (value);
  high = CONST_DOUBLE_HIGH (value);

  if (low & 0x80000000)
    {
      return (((unsigned HOST_WIDE_INT) low <= 0xffffffff && high == 0)
	      || (((low & - (unsigned HOST_WIDE_INT) 0x80000000)
		   == - (unsigned HOST_WIDE_INT) 0x80000000)
		  && high == -1));
    }
  else
    {
      return (unsigned HOST_WIDE_INT) low <= 0x7fffffff && high == 0;
    }
}

/* Do any needed setup for a variadic function.  For the ARC, we must
   create a register parameter block, and then copy any anonymous arguments
   in registers to memory.

   CUM has not been updated for the last named argument which has type TYPE
   and mode MODE, and we rely on this fact.

   We do things a little weird here.  We're supposed to only allocate space
   for the anonymous arguments.  However we need to keep the stack eight byte
   aligned.  So we round the space up if necessary, and leave it to va-arc.h
   to compensate.  */

void
arc_setup_incoming_varargs (CUMULATIVE_ARGS *args_so_far,
			    enum machine_mode mode,
			    tree type ATTRIBUTE_UNUSED,
			    int *pretend_size,
			    int no_rtl)
{
  int first_anon_arg;
  CUMULATIVE_ARGS next_cum;

  /* We must treat `__builtin_va_alist' as an anonymous arg.  */
  
  next_cum = *args_so_far;
  arc_function_arg_advance (&next_cum, mode, type, 1);
  first_anon_arg = next_cum;

  if (first_anon_arg < MAX_ARC_PARM_REGS && !no_rtl)
    {
      /* First anonymous (unnamed) argument is in a reg */

      /* Note that first_reg_offset < MAX_ARC_PARM_REGS.  */
      int first_reg_offset = first_anon_arg;

      /* Size in words to "pretend" allocate.  */
      int size = MAX_ARC_PARM_REGS - first_reg_offset;

      /* Extra slop to keep stack eight byte aligned.  */
      int align_slop = size & 1;
      rtx regblock;

      regblock = gen_rtx_MEM (BLKmode,
			  plus_constant (arg_pointer_rtx,
					 FIRST_PARM_OFFSET (0)
					 + align_slop * UNITS_PER_WORD));
      move_block_from_reg (first_reg_offset, regblock,
			   MAX_ARC_PARM_REGS - first_reg_offset);

      *pretend_size = ((MAX_ARC_PARM_REGS - first_reg_offset + align_slop)
		       * UNITS_PER_WORD);
    } else {
      /* First anonymous argument already on stack - nothing to be done */
    } /* if */
}

/* Cost functions.  */

/* Provide the costs of an addressing mode that contains ADDR.
   If ADDR is not a valid address, its cost is irrelevant.  */

int
arc_address_cost (rtx addr)
{
  switch (GET_CODE (addr))
    {
    case REG :
      return arc_size_opt_level < 1 || satisfies_constraint_Rcq (addr) ? 0 : 1;
    case PRE_INC: case PRE_DEC: case POST_INC: case POST_DEC:
    case PRE_MODIFY: case POST_MODIFY:
      return arc_size_opt_level >= 1;

    case LABEL_REF :
    case SYMBOL_REF :
    case CONST :
      /* Most likely needs a LIMM.  */
      return COSTS_N_INSNS (1);

    case PLUS :
      {
	register rtx plus0 = XEXP (addr, 0);
	register rtx plus1 = XEXP (addr, 1);

	if (GET_CODE (plus0) != REG
	    && (GET_CODE (plus0) != MULT
		|| !CONST_INT_P (XEXP (plus0, 1))
		|| (INTVAL (XEXP (plus0, 1)) != 2
		    && INTVAL (XEXP (plus0, 1)) != 4)))
	  break;

	switch (GET_CODE (plus1))
	  {
	  case CONST_INT :
	    return (TARGET_A4
		    ? (SMALL_INT (plus1) ? 1 : 2)
		    : !RTX_OK_FOR_OFFSET_P (SImode, plus1)
		    ? COSTS_N_INSNS (1)
		    : arc_size_opt_level < 1
		    ? 0
                    : (satisfies_constraint_Rcq (plus0)
		       && satisfies_constraint_O (plus1))
		    ? 0
		    : 1);
	  case REG:
	    return (arc_size_opt_level < 1 ? 0
		    : (satisfies_constraint_Rcq (plus0)
		       && satisfies_constraint_Rcq (plus1))
		    ? 0 : 1);
	  case CONST :
	  case SYMBOL_REF :
	  case LABEL_REF :
	    return COSTS_N_INSNS (1);
	  default:
	    break;
	  }
	break;
      }
    default:
      break;
    }

  return 4;
}

/* Function prologue/epilogue handlers.  */

/* ARCtangent-A4 stack frames look like:

             Before call                       After call
        +-----------------------+       +-----------------------+
        |                       |       |                       |
   high |  local variables,     |       |  local variables,     |
   mem  |  reg save area, etc.  |       |  reg save area, etc.  |
        |                       |       |                       |
        +-----------------------+       +-----------------------+
        |                       |       |                       |
        |  arguments on stack.  |       |  arguments on stack.  |
        |                       |       |                       |
 SP+16->+-----------------------+FP+48->+-----------------------+
        | 4 word save area for  |       |  reg parm save area,  |
        | return addr, prev %fp |       |  only created for     |    
  SP+0->+-----------------------+       |  variable argument    |    
                                        |  functions            |    
                                 FP+16->+-----------------------+    
                                        | 4 word save area for  |    
                                        | return addr, prev %fp |    
                                  FP+0->+-----------------------+    
                                        |                       |    
                                        |  local variables      |    
                                        |                       |    
                                        +-----------------------+    
                                        |                       |    
                                        |  register save area   |    
                                        |                       |    
                                        +-----------------------+    
                                        |                       |    
                                        |  alloca allocations   |    
                                        |                       |    
                                        +-----------------------+    
                                        |                       |    
                                        |  arguments on stack   |    
                                        |                       |    
                                 SP+16->+-----------------------+
   low                                  | 4 word save area for  |    
   memory                               | return addr, prev %fp |    
                                  SP+0->+-----------------------+    

ARCompact stack frames look like:

           Before call                     After call
  high  +-----------------------+       +-----------------------+
  mem   |  reg parm save area   |       | reg parm save area    |
        |  only created for     |       | only created for      |
        |  variable arg fns     |       | variable arg fns      |
    AP  +-----------------------+       +-----------------------+
        |  return addr register |       | return addr register  |
        |  (if required)        |       | (if required)         |
        +-----------------------+       +-----------------------+
        |                       |       |                       |
        |  reg save area        |       | reg save area         |
        |                       |       |                       |
        +-----------------------+       +-----------------------+
        |  frame pointer        |       | frame pointer         |
        |  (if required)        |       | (if required)         |
    FP  +-----------------------+       +-----------------------+
        |                       |       |                       |
        |  local/temp variables |       | local/temp variables  |
        |                       |       |                       |
        +-----------------------+       +-----------------------+    
        |                       |       |                       |    
        |  arguments on stack   |       | arguments on stack    |    
        |                       |       |                       |    
    SP  +-----------------------+       +-----------------------+    
                                        | reg parm save area    |
                                        | only created for      |
                                        | variable arg fns      |
                                    AP  +-----------------------+
                                        | return addr register  |
                                        | (if required)         |
                                        +-----------------------+
                                        |                       |
                                        | reg save area         |
                                        |                       |
                                        +-----------------------+
                                        | frame pointer         |
                                        | (if required)         |
                                    FP  +-----------------------+
                                        |                       |
                                        | local/temp variables  |
                                        |                       |
                                        +-----------------------+    
                                        |                       |    
                                        | arguments on stack    |    
  low                                   |                       |    
  mem                               SP  +-----------------------+    

Notes:
1) The "reg parm save area" does not exist for non variable argument fns.
   The "reg parm save area" can be eliminated completely if we created our
   own va-arc.h, but that has tradeoffs as well (so it's not done).  */

/* Structure to be filled in by arc_compute_frame_size with register
   save masks, and offsets for the current function.  */
struct arc_frame_info
{
  unsigned int total_size;	/* # bytes that the entire frame takes up.  */
  unsigned int extra_size;	/* # bytes of extra stuff.  */
  unsigned int pretend_size;	/* # bytes we push and pretend caller did.  */
  unsigned int args_size;	/* # bytes that outgoing arguments take up.  */
  unsigned int reg_size;	/* # bytes needed to store regs.  */
  unsigned int var_size;	/* # bytes that variables take up.  */
  unsigned int reg_offset;	/* Offset from new sp to store regs.  */
  unsigned int gmask;		/* Mask of saved gp registers.  */
  int          initialized;	/* Nonzero if frame size already calculated.  */
};

/* Current frame information calculated by arc_compute_frame_size.  */
static struct arc_frame_info current_frame_info;

/* Zero structure to initialize current_frame_info.  */
static struct arc_frame_info zero_frame_info;

/* Type of function DECL.

   The result is cached.  To reset the cache at the end of a function,
   call with DECL = NULL_TREE.  */

enum arc_function_type
arc_compute_function_type (tree decl)
{
  tree a;
  /* Cached value.  */
  static enum arc_function_type fn_type = ARC_FUNCTION_UNKNOWN;
  /* Last function we were called for.  */
  static tree last_fn = NULL_TREE;

  /* Resetting the cached value?  */
  if (decl == NULL_TREE)
    {
      fn_type = ARC_FUNCTION_UNKNOWN;
      last_fn = NULL_TREE;
      return fn_type;
    }

  if (decl == last_fn && fn_type != ARC_FUNCTION_UNKNOWN)
    return fn_type;

  /* Assume we have a normal function (not an interrupt handler).  */
  fn_type = ARC_FUNCTION_NORMAL;

  /* Now see if this is an interrupt handler.  */
  for (a = DECL_ATTRIBUTES (current_function_decl);
       a;
       a = TREE_CHAIN (a))
    {
      tree name = TREE_PURPOSE (a), args = TREE_VALUE (a);

      if (name == get_identifier ("__interrupt__")
	  && list_length (args) == 1
	  && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
	{
	  tree value = TREE_VALUE (args);

	  if (!strcmp (TREE_STRING_POINTER (value), "ilink1"))
	    fn_type = ARC_FUNCTION_ILINK1;
	  else if (!strcmp (TREE_STRING_POINTER (value), "ilink2"))
	    fn_type = ARC_FUNCTION_ILINK2;
	  else
	    gcc_unreachable ();
	  break;
	}
    }

  last_fn = decl;
  return fn_type;
}

#define ILINK1_REGNUM 29
#define ILINK2_REGNUM 30
#define RETURN_ADDR_REGNUM 31
#define FRAME_POINTER_MASK (1 << (FRAME_POINTER_REGNUM))
#define RETURN_ADDR_MASK (1 << (RETURN_ADDR_REGNUM))

/* Tell prologue and epilogue if register REGNO should be saved / restored.
   The return address and frame pointer are treated separately.
   Don't consider them here.
   Addition for pic: The gp register needs to be saved if the current
   function changes it to access gotoff variables.
   FIXME: This will not be needed if we used some arbitrary register
   instead of r26.
*/
#define MUST_SAVE_REGISTER(regno, interrupt_p) \
(((regno) != RETURN_ADDR_REGNUM && (regno) != FRAME_POINTER_REGNUM \
 && (regs_ever_live[regno] && (!call_used_regs[regno] || interrupt_p))) ||\
 (flag_pic && current_function_uses_pic_offset_table && regno == PIC_OFFSET_TABLE_REGNUM) )

#define MUST_SAVE_RETURN_ADDR \
  (regs_ever_live[RETURN_ADDR_REGNUM])

/*
 Returns non-zero if there are registers to be saved or loaded using millicode thunks.
 start_call.. end_call is the range of registers loaded.
 Present Limitation:
    > Only returns one range of registers.
*/
static int
arc_compute_millicode_save_restore_regs(unsigned int gmask,  short *start_call,  short *end_call)
{
  if(gmask)
    {
	  unsigned int gmask_millicode_thunk=gmask;
	  int regno;
	  short  last_call;
	  *start_call = last_call = *end_call = -1;

	  for (regno=0; regno <=31; regno++)
	    {
	      if (gmask_millicode_thunk & (1L << regno))
		{
		  /* Aragorn: Abandon millicode thunks generation if
		     some other registers (r0-r12 and r26-r32) also
		     included */
		  if ((regno <13) || (regno > 25))
		    continue;

		  /* Aragorn: For register saves like
		     13,14,16 (note the missing 15)
		     we dont call the thunk.*/
		  if ((last_call != (regno -1)) && (last_call>0))
		    {
		      *end_call = last_call;
		      regno = 32;
		      break;
		    }

		  if (*start_call  < 0)
		    *start_call = regno;

		  *end_call = last_call;
		  last_call = regno;
		}
	      else
		*end_call = last_call;
	    }
	  if (*start_call != -1)
	    return 1;
    }
  return 0;
}

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.

   SIZE is the size needed for local variables.  */

unsigned int
arc_compute_frame_size (int size)	/* size = # of var. bytes allocated.  */
{
  int regno;
  unsigned int total_size, var_size, args_size, pretend_size, extra_size;
  unsigned int reg_size, reg_offset;
  unsigned int gmask;
  enum arc_function_type fn_type;
  int interrupt_p;

  size = ARC_STACK_ALIGN(size);

  /* 1) Size of locals and temporaries */
  var_size	= size;

  /* 2) Size of outgoing arguments */
  args_size	= current_function_outgoing_args_size;

  /* 3) Calculate space needed for saved registers.
     ??? We ignore the extension registers for now.  */

  /* See if this is an interrupt handler.  Call used registers must be saved
     for them too.  */

  reg_size = 0;
  gmask = 0;
  fn_type = arc_compute_function_type (current_function_decl);
  interrupt_p = ARC_INTERRUPT_P (fn_type);

  for (regno = 0; regno <= 31; regno++)
    {
      if (MUST_SAVE_REGISTER (regno, interrupt_p))
	{
	  reg_size += UNITS_PER_WORD;
	  gmask |= 1 << regno;
	}
    }

  /* 4) Space for back trace data structure.

        For ARCtangent-A4:
          <return addr reg size> + <fp size> + <static link reg size> +
          <reserved-word>

        For ARCompact:
          <return addr reg size> (if required) + <fp size> (if required)
  */
  /* Saving blink reg in case of leaf function for millicode thunk calls */
  if (optimize_size && !TARGET_NO_MILLICODE_THUNK_SET)
    {
      short start_call,end_call;
      if (leaf_function_p()
	  && arc_compute_millicode_save_restore_regs(gmask, &start_call, 
						     &end_call))
	{
	  if (((end_call - start_call) >= 3) 
	      && !MUST_SAVE_RETURN_ADDR)/* Note: generate calls to millicode
					   thunks only if saving 4 registers
					   (this is when 
					   (end_call - start_call) >=3)*/
  	    regs_ever_live[RETURN_ADDR_REGNUM] = 1;
	}
    }

  if (TARGET_A4)
    {
      extra_size = 16;
    }
  else
    {
      extra_size = 0;
      if (MUST_SAVE_RETURN_ADDR)
        extra_size = 4;
      if (frame_pointer_needed)
        extra_size += 4;
    }

  /* 5) Space for variable arguments passed in registers */
  pretend_size	= current_function_pretend_args_size;

  /* Ensure everything before the locals is aligned appropriately */
  if (TARGET_ARCOMPACT)
    { 
       unsigned int extra_plus_reg_size;
       unsigned int extra_plus_reg_size_aligned;

       extra_plus_reg_size = extra_size + reg_size;
       extra_plus_reg_size_aligned = ARC_STACK_ALIGN(extra_plus_reg_size);
       reg_size = extra_plus_reg_size_aligned - extra_size;
    } /* if */

  /* Compute total frame size */
  total_size = var_size + args_size + extra_size + pretend_size + reg_size;

  /* If the only space to allocate is the fp/blink save area this is an
     empty frame.  However, if we'll be making a function call we need to
     allocate a stack frame for our callee's fp/blink save area.  */
  if (total_size == extra_size && !MUST_SAVE_RETURN_ADDR)
    total_size = extra_size = 0;

  total_size = ARC_STACK_ALIGN (total_size);

  /* Compute offset of register save area from stack pointer:
     A4 Frame: pretend_size var_size reg_size args_size extra_size <--sp
     A5 Frame: pretend_size <blink> reg_size <fp> var_size args_size <--sp
  */
  if (TARGET_A4)
     reg_offset = total_size - (pretend_size + var_size + reg_size);
  else
     reg_offset = total_size - (pretend_size + reg_size + extra_size) +
                  (frame_pointer_needed ? 4 : 0);

  /* Save computed information.  */
  current_frame_info.total_size   = total_size;
  current_frame_info.extra_size   = extra_size;
  current_frame_info.pretend_size = pretend_size;
  current_frame_info.var_size     = var_size;
  current_frame_info.args_size    = args_size;
  current_frame_info.reg_size	  = reg_size;
  current_frame_info.reg_offset	  = reg_offset;
  current_frame_info.gmask	  = gmask;
  current_frame_info.initialized  = reload_completed;

  /* Ok, we're done.  */
  return total_size;
}

/* Common code to save/restore registers.  */
/* If PARITY is non-negative, it is zero if we should emit an even
   number of short stores, and odd otherwise.
   if it is negative, the parity doesn't matter.  */
static void
arc_save_restore (FILE *file, const char *base_reg, unsigned int offset,
		  unsigned int gmask, const char *op, int parity,
		  int *first_offset)
{
  int regno;
                      
  if (gmask)      
    {
      int compact_regs;

      /* Millicode thunks implementation:
	 Generates calls to millicodes for registers starting from r13 to r25
	 Present Limitations:
            > Only one range supported. The remaining regs will have the ordinary
	    st and ld instructions for store and loads. Hence a gmask asking
	    to store r13-14, r16-r25 will only generate calls to store and
	    load r13 to r14 while store and load insns will be generated for
	    r16 to r25 in the prologue and epilogue respectively.
    
            > Presently library only supports register ranges starting from
	    r13
      */
      if (optimize_size && !TARGET_NO_MILLICODE_THUNK_SET)
	{
	  short start_call, end_call;

	  if (arc_compute_millicode_save_restore_regs(gmask, &start_call, &end_call)
	      && !(start_call<0 || end_call<0 || (start_call == end_call)))
	    {	  /* Now do the saving/restoring from start_call to end_call*/
	      int is_leaf=leaf_function_p();
	      fprintf (file, "\t%s is_leaf = %d\n", ASM_COMMENT_START, is_leaf);


	      if (is_leaf && !MUST_SAVE_RETURN_ADDR)
		{
		  if (!((end_call - start_call)>=3))
		    start_call = end_call + 1;		    
		}
	      if (start_call < end_call)
		{
		  if (*first_offset)
		    {
		      /* "reg_size" won't be more than 127 */
		      gcc_assert (abs(*first_offset <= 127));
		      fprintf (file, "\t%s_s %s,%s,%d\n",
			       *first_offset < 0 ? "sub" : "add",
			       base_reg, base_reg,
			       (*first_offset < 0
				? -*first_offset : *first_offset));
		      if (doing_dwarf)
			{
			  cfa_offset -= *first_offset;
			  dwarf2out_def_cfa  ("", STACK_POINTER_REGNUM,
					      cfa_offset);
			}
		      *first_offset = 0;
		    }
		  fprintf (file, "\tbl __%s_%s_to_%s\n",
			   op, reg_names[start_call], reg_names[end_call]);
		}


	      while(start_call <= end_call)
		{
		  gmask = gmask & ~(1L << start_call);
		  offset += UNITS_PER_WORD;
		  start_call++;
		}
	    }
	}

      compact_regs = gmask & 0xf00f;
      if (parity >= 0)
	{
	  if (*first_offset != 0)
	    compact_regs &= compact_regs - 1;
	  /* If there is an odd number of compact registers to save,
	     exclude one from using a short insn to keep code alignment.  */
	  parity ^= compact_regs ^ (compact_regs >> 1);

	  parity ^= parity >> 2;
	  parity ^= parity >> 12;
	  compact_regs &= compact_regs - (parity & 1);
	}
      for (regno = 0; regno <= 31; regno++)
	{
	  if ((gmask & (1L << regno)) != 0)
	    {
	      if (*first_offset)
		{
		  gcc_assert (!offset);
		  fprintf (file, "\t%s.a %s,[%s,%d]\n", op,  
			   reg_names[regno], base_reg, *first_offset);
		  if (doing_dwarf)
		    {
		      cfa_offset -= *first_offset;
		      dwarf2out_def_cfa  ("", STACK_POINTER_REGNUM, cfa_offset);
		    }
		  *first_offset = 0;
		}

		/* Dwarf2 info  has to be updated for this change */
		
	      else if (SMALL_INT(offset))
		{
		    fprintf (file, "\t%s%s %s,[%s,%d]\n",
			     op,  
			     ((TARGET_ARCOMPACT
			       && (compact_regs & (1L << regno))) ?"_s":""),
			     reg_names[regno], base_reg, offset);
		    
		}
		else
		{
		    fprintf (file, "\tadd %s,%s,%d\n",ARC_TEMP_SCRATCH_REG, base_reg, offset);
		    fprintf (file, "\t%s %s,[%s,0]\n",
			     op, reg_names[regno], ARC_TEMP_SCRATCH_REG);
		}

		if(doing_dwarf)
		{
		    /* Assuming that the base_reg is SP, which is always
		       true. Otherwise cfa_offset should not be used */
		    dwarf2out_reg_save("",regno,-(cfa_offset - offset));
		}
		offset += UNITS_PER_WORD;
	    } /* if */
	} /* for */
    }/* if */
} /* arc_save_restore */


/* Target hook to assemble an integer object.  The ARC version needs to
   emit a special directive for references to labels and function
   symbols. */  

static bool
arc_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
    if (size == UNITS_PER_WORD && aligned_p
	&& ((GET_CODE (x) == SYMBOL_REF && ARC_FUNCTION_NAME_PREFIX_P(* (XSTR (x, 0))))
	    || GET_CODE (x) == LABEL_REF))
    {
	fputs ("\t.word\t", asm_out_file);
	/* %st is to be generated only for A4 */
	if( TARGET_A4 )
	    fputs("%st(", asm_out_file);
	output_addr_const (asm_out_file, x);
	if( TARGET_A4 )
	    fputs (")", asm_out_file);
	fputs("\n", asm_out_file);
	return true;
    }
  return default_assemble_integer (x, size, aligned_p);
}

/* Set up the stack and frame pointer (if desired) for the function.  */
void
arc_output_function_prologue (FILE *file,HOST_WIDE_INT size)
{
  const char *sp_str = reg_names[STACK_POINTER_REGNUM];
  const char *fp_str = reg_names[FRAME_POINTER_REGNUM];
  unsigned int gmask = current_frame_info.gmask;
  /*  unsigned int frame_pointer_offset;*/
  unsigned int frame_size_to_allocate;
  enum arc_function_type fn_type = arc_compute_function_type (current_function_decl);
  /* Use short insn if we can restore alignment by emitting an appropriate
     number of short stores.  Since the first store will use a PRE_MODIFY,
     we need a second register to store.
     (FIXME: change the stack layout so that we rather store a high
     register with the PRE_MODIFY.)
     We also use short insn if we care more about code size than (infrequent)
     misalign penalties.  */
  int use_short
    = ((gmask & 0xf00f) & ((gmask & 0xf00f) - 1)) || arc_size_opt_level >= 1;
  int parity = arc_size_opt_level >= 1 ? -1 : 0;
  int first_offset = 0;


 /* Initializations necessary for debugging information generation */
  const char *label = dwarf2out_cfi_label ();
  cfa_offset=0;
  doing_dwarf = dwarf2out_do_frame();
  if(doing_dwarf)
    {
      dwarf2out_def_cfa(label,STACK_POINTER_REGNUM, cfa_offset);
      /* Call dwarf2 _ reg save directly to tell it that blink is in
	 register itself
      */
      dwarf2out_return_reg("" , RETURN_ADDR_REGNUM);
    }

  /* If this is an interrupt handler, set up our stack frame.
     ??? Optimize later.  */
  if (ARC_INTERRUPT_P (fn_type))
    {
      fprintf (file, "\t%s interrupt handler\n", ASM_COMMENT_START);
      if (TARGET_A4)
        fprintf (file, "\tsub %s,%s,16\n", sp_str, sp_str);
    }

  size = ARC_STACK_ALIGN (size);

  /* Compute/get total frame size */
  size = (!current_frame_info.initialized
	   ? arc_compute_frame_size (size)
	   : current_frame_info.total_size);

  /* Keep track of frame size to be allocated */
  frame_size_to_allocate = size;

  /* This is only for the human reader.  */
  fprintf (file, "\t%s BEGIN PROLOGUE %s vars= %d, regs= %d, args= %d, extra= %d\n",
	   ASM_COMMENT_START, ASM_COMMENT_START,
	   current_frame_info.var_size,
	   current_frame_info.reg_size / 4,
	   current_frame_info.args_size,
	   current_frame_info.extra_size);

  /* These cases shouldn't happen.  Catch them now.  */
  gcc_assert (!(size == 0 && gmask));

  /* Allocate space for register arguments if this is a variadic function.  */
  if (current_frame_info.pretend_size != 0)
    {
       /* Ensure pretend_size is multiple of word_size and maximum of 
          8 * word_size */
      gcc_assert ((current_frame_info.pretend_size & 0x3) == 0 );
      gcc_assert (current_frame_info.pretend_size <= 32);

       if (TARGET_A4)
         {
           fprintf (file, "\tsub %s,%s,%d\n",
                    sp_str, sp_str, current_frame_info.pretend_size);
	   if (doing_dwarf)
	   {
	       /* cfa_offset = CFA - SP, so cfa_offset will be positive */
	       cfa_offset += current_frame_info.pretend_size;
	       dwarf2out_def_cfa ("", STACK_POINTER_REGNUM, cfa_offset);
	   }

         }
      else /* TARGET_ARCOMPACT */
	{
	  if (use_short)
	    {
	      fprintf (file, "\tsub_s %s,%s,%d\n",
		       sp_str, sp_str, current_frame_info.pretend_size);
	      parity ^= 1;
	    }
          else
            fprintf (file, "\tsub %s,%s,%d\n",
                     sp_str, sp_str, current_frame_info.pretend_size);
	   if (doing_dwarf)
	   {
	       /* cfa_offset = CFA - SP, so cfa_offset will be positive */
	       cfa_offset += current_frame_info.pretend_size;
	       dwarf2out_def_cfa ("", STACK_POINTER_REGNUM, cfa_offset);
	   }
	
         }

       frame_size_to_allocate -= current_frame_info.pretend_size;
    }
    
  /* The home-grown ABI says link register is saved first. */
  if (MUST_SAVE_RETURN_ADDR)
    {
      if (TARGET_A4)
        {
          /* Save return address register in the space allocated by caller for
             backtrace data structure */
          fprintf (file, "\tst %s,[%s,%d]\n",
                   reg_names[RETURN_ADDR_REGNUM], sp_str, UNITS_PER_WORD);
	  if(doing_dwarf)
	  {
	      dwarf2out_reg_save ("", RETURN_ADDR_REGNUM, -cfa_offset + UNITS_PER_WORD);
	  }
    
        }
      else /* TARGET_ARCOMPACT */
        {
          /* Space not yet allocated for return addr reg; push */
          if (use_short)
	    {
              fprintf (file, "\tpush_s %s\n", reg_names[RETURN_ADDR_REGNUM]);
	      parity ^= 1;
	    }
          else
            fprintf (file, "\tst.a %s,[%s,%d]\n",
                     reg_names[RETURN_ADDR_REGNUM], sp_str, -UNITS_PER_WORD);
	  cfa_offset += UNITS_PER_WORD;
	  if(doing_dwarf)
	  {
	      dwarf2out_def_cfa  ("", STACK_POINTER_REGNUM, cfa_offset);
	      dwarf2out_reg_save ("", RETURN_ADDR_REGNUM, -cfa_offset);
	  }
	  

          frame_size_to_allocate -= UNITS_PER_WORD;

        }  /* if */
    } /* MUST_SAVE_RETURN_ADDR */
 else
  {
      /* Call dwarf2 _ reg save directly to tell it that blink is in
	 register itself
      */
    if (doing_dwarf)
      dwarf2out_return_reg("" , RETURN_ADDR_REGNUM);
  }

  /* Save any needed call-saved regs (and call-used if this is an
     interrupt handler) for ARCompact ISA.  */
  if (TARGET_ARCOMPACT && current_frame_info.reg_size)
    {
      first_offset = -current_frame_info.reg_size;
      /* N.B. FRAME_POINTER_MASK and RETURN_ADDR_MASK are cleared in gmask.  */
      arc_save_restore (file, sp_str, 0, gmask, "st", parity, &first_offset);
      frame_size_to_allocate -= current_frame_info.reg_size;
    } /* if */


  /* Save frame pointer if needed */
  if (frame_pointer_needed)
    {
      if (TARGET_A4)
        {
          fprintf (file, "\tst %s,[%s]\n", fp_str, sp_str);
	  if(doing_dwarf)
	  {
	      dwarf2out_reg_save ("", FRAME_POINTER_REGNUM, -cfa_offset);
	  }

        }
      else /* TARGET_ARCOMPACT */
        {
          fprintf (file, "\tst.a %s,[%s,%d]\n", fp_str, sp_str,
                   -UNITS_PER_WORD + first_offset);
          frame_size_to_allocate -= UNITS_PER_WORD;
	  if (doing_dwarf)
	    {
	      cfa_offset += UNITS_PER_WORD - first_offset;
 	      if (current_function_calls_alloca) 
		dwarf2out_def_cfa ("", FRAME_POINTER_REGNUM ,cfa_offset);
 	      else 
 		dwarf2out_def_cfa ("", STACK_POINTER_REGNUM, cfa_offset); 
	      dwarf2out_reg_save ("", FRAME_POINTER_REGNUM, -cfa_offset);
	    }
	  first_offset = 0;
        } /* if */
      fprintf (file, "\tmov %s,%s\n", fp_str, sp_str);
      if(doing_dwarf)
	dwarf2out_def_cfa ("", FRAME_POINTER_REGNUM ,cfa_offset);
    } /* if */

  /* ??? We don't handle the case where the saved regs are more than 252
     bytes away from sp.  This can be handled by decrementing sp once, saving
     the regs, and then decrementing it again.  The epilogue doesn't have this
     problem as the `ld' insn takes reg+limm values (though it would be more
     efficient to avoid reg+limm).  */

  frame_size_to_allocate -= first_offset;
  /* Allocate the stack frame.  */
  if (frame_size_to_allocate > 0)
    {
      /* Ensure frame size yet to allocate is multiple of 8 */
      gcc_assert ((frame_size_to_allocate & 0x3) == 0);

      if (arc_size_opt_level >= 1 && frame_size_to_allocate < 128)
	  fprintf (file, "\tsub_s %s,%s,%d\n",
                 sp_str, sp_str, frame_size_to_allocate);
      else
          fprintf (file, "\tsub %s,%s,%d\n",
                 sp_str, sp_str, frame_size_to_allocate);
      if(doing_dwarf)
      {
	  if (current_function_calls_alloca || frame_pointer_needed)
	      dwarf2out_def_cfa ("", FRAME_POINTER_REGNUM ,cfa_offset);
	  else
	  {
	      cfa_offset += frame_size_to_allocate;
	      dwarf2out_def_cfa ("", STACK_POINTER_REGNUM ,cfa_offset);
	  }
      }
      
    }
    
  /* For ARCtangent-A4, save any needed call-saved regs (and call-used
     if this is an interrupt handler).
     This is already taken care for ARCompact architectures */

  if (TARGET_A4)
    {
      arc_save_restore (file, sp_str, current_frame_info.reg_offset,
                        /* The zeroing of these two bits is unnecessary,
                           but leave this in for clarity.  */
                        gmask & ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK), "st",
			-1, 0);
    } /* if */

  /* Setup the gp register, if needed */
  if (current_function_uses_pic_offset_table)
    fprintf (file, "\tadd gp, pcl, @_DYNAMIC@gotpc\n");

  fprintf (file, "\t%s END PROLOGUE\n", ASM_COMMENT_START);
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs. */

void
arc_output_function_epilogue (FILE *file,HOST_WIDE_INT size)
{
  rtx epilogue_delay = 0;
  int noepilogue = FALSE;
  bool sibcall_epilogue = FALSE;
  enum arc_function_type fn_type = arc_compute_function_type (current_function_decl);

  /* This is only for the human reader.  */
  fprintf (file, "\t%s EPILOGUE\n", ASM_COMMENT_START);

  size = ARC_STACK_ALIGN (size);
  size = (!current_frame_info.initialized
	   ? arc_compute_frame_size (size)
	   : current_frame_info.total_size);

  /* check if this is the sibcall_epilogue being emitted */
  {
    /* Get the current instruction being emitted. If this is a sibcall instruction
       then we are in sibcall_epilogue. For a normal epilogue, current_output_insn 
       is NULL. */
    rtx insn = current_output_insn;
    
    /* This should never happen, as insn is non-zero only for sibcall instructions */
    gcc_assert (insn == NULL || next_nonnote_insn (insn));

    /* Set a flag which will be used to decide whether to jmp back on blink, or not */
    if (insn && next_nonnote_insn (insn) && SIBLING_CALL_P (next_nonnote_insn (insn)))
	sibcall_epilogue = TRUE;
  }

  /* The current_function_epilogue_delay_list is meaningful only in the final epilogue, 
     and not for sibcall epilogues */
  if (sibcall_epilogue == FALSE)
    epilogue_delay  = current_function_epilogue_delay_list;

  if ((sibcall_epilogue == FALSE) && epilogue_delay == 0)
    {
      rtx insn = get_last_insn ();

      /* If the last insn was a BARRIER, we don't have to write any code
	 because a jump (aka return) was put there.  */
      if (GET_CODE (insn) == NOTE)
	insn = prev_nonnote_insn (insn);
      if (insn && GET_CODE (insn) == BARRIER)
	noepilogue = TRUE;
    }
  
  if (!noepilogue)
    {
      unsigned int pretend_size = current_frame_info.pretend_size;
      unsigned int frame_size; 
      unsigned int size_to_deallocate; 
      int restored, fp_restored_p;
      int can_trust_sp_p = !current_function_calls_alloca;
      const char *sp_str = reg_names[STACK_POINTER_REGNUM];
      const char *fp_str = reg_names[FRAME_POINTER_REGNUM];
      int first_offset = 0;

      size_to_deallocate = size;

      if (TARGET_A4)
        frame_size = size - pretend_size;
      else
        frame_size = size - (pretend_size +
                             current_frame_info.reg_size + 
                             current_frame_info.extra_size);

      /* ??? There are lots of optimizations that can be done here.
	 EG: Use fp to restore regs if it's closer.
	 Maybe in time we'll do them all.  For now, always restore regs from
	 sp, but don't restore sp if we don't have to.  */

      if (!can_trust_sp_p)
	{
	  gcc_assert (frame_pointer_needed);

	  fprintf (file,"\tsub %s,%s,%d\t\t%s sp not trusted here\n",
		   sp_str, fp_str, frame_size, ASM_COMMENT_START);
	  /*FIXME:: ???  Assuming fp and sp are same at this position. 
	    If not same then the dwarf information generation is incorrect. */
	  if(doing_dwarf)
	    {
	      if (current_function_calls_alloca || frame_pointer_needed)
		  dwarf2out_def_cfa("",FRAME_POINTER_REGNUM,cfa_offset);
	      else
		{
		  cfa_offset=-frame_size;
	      	  dwarf2out_def_cfa("",STACK_POINTER_REGNUM,cfa_offset);
		}
	    }
	}

      /* Restore stack pointer to the beginning of saved register area for
         ARCompact ISA */
      if (TARGET_ARCOMPACT && frame_size)
	{
	  if ((optimize_size && !TARGET_NO_MILLICODE_THUNK_SET)
	      || (current_function_calls_alloca || frame_pointer_needed))
	    {
	      if (frame_size < 128)
		fprintf (file, "\tadd_s %s,%s,%d\n", sp_str, sp_str,
			 frame_size);
	      else
		fprintf (file, "\tadd %s,%s,%d\n", sp_str, sp_str, frame_size);
	      if (doing_dwarf)
		{
		  if (current_function_calls_alloca || frame_pointer_needed)
		    dwarf2out_def_cfa("",FRAME_POINTER_REGNUM,cfa_offset);
		  else
		    {
		      cfa_offset-=frame_size;
		      dwarf2out_def_cfa("",STACK_POINTER_REGNUM,cfa_offset);
		    }
		}
	    }
	  else
	    first_offset = frame_size;
          size_to_deallocate -= frame_size;
        } /* if */

      /* Restore any saved registers. */
      if (TARGET_A4)
        {
          if (current_frame_info.reg_size)
            arc_save_restore (file, sp_str, current_frame_info.reg_offset,
                            /* The zeroing of these two bits is unnecessary,
                               but leave this in for clarity.  */
                            current_frame_info.gmask
                            & ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK),
			    "ld", -1, 0);
          if (MUST_SAVE_RETURN_ADDR)
            fprintf (file, "\tld %s,[%s,%d]\n", reg_names[RETURN_ADDR_REGNUM],
                     (frame_pointer_needed ? fp_str : sp_str),
                     UNITS_PER_WORD + (frame_pointer_needed ? 0 : frame_size));
        }
      else /* TARGET_ARCOMPACT */
        {

          if (frame_pointer_needed)
            {
              fprintf (file, "\tld.ab %s,[%s,%d]\n", fp_str,sp_str,
                       UNITS_PER_WORD);
              size_to_deallocate -= UNITS_PER_WORD;
	      if(doing_dwarf)
	      {
		if (current_function_calls_alloca || frame_pointer_needed)
		  dwarf2out_def_cfa("",FRAME_POINTER_REGNUM,cfa_offset);
		else
		  {
		    cfa_offset-=UNITS_PER_WORD;
		    dwarf2out_def_cfa("",STACK_POINTER_REGNUM,cfa_offset);
		  }
	      }
            } /* if */

	  /* Load blink after the calls to thunk calls in case of
	     optimize size.  
	  */
	  if(optimize_size && !TARGET_NO_MILLICODE_THUNK_SET)
	    {
	      if (current_frame_info.reg_size)
		{
		  arc_save_restore (file, sp_str,  0,
			  /* The zeroing of these two bits is unnecessary,
			     but leave this in for clarity.  */
				  current_frame_info.gmask
				  & ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK),
				    "ld", -1, &first_offset);
#if 0
		  /* The thunk takes care of the return, so no point
		   * continuing with the epilogue any more*/
		  if(arc_compute_millicode_save_restore_regs(current_frame_info.gmask
							     & ~(FRAME_POINTER_MASK 
								 | RETURN_ADDR_MASK), 
							     &dummy_start, &dummy_end)
		     && (dummy_end - dummy_start >= 3))
		     return;
#endif
		}
	    }
	  if (MUST_SAVE_RETURN_ADDR)
	    {
	      if (current_frame_info.gmask
		  || (!SMALL_INT (current_frame_info.reg_size + first_offset)
		      && satisfies_constraint_C2a (GEN_INT (size_to_deallocate
							    + first_offset))))
		{
		  gcc_assert (!((current_frame_info.reg_size + first_offset)
				& 3));
		  fprintf (file, "\tld.as %s,[%s,%d]\n",
			   reg_names[RETURN_ADDR_REGNUM], sp_str,
			   ((current_frame_info.reg_size + first_offset)
			    / 4));
		}
	      else
		{
		  fprintf (file, "\tld.a %s,[%s,%d]\n",
			   reg_names[RETURN_ADDR_REGNUM], sp_str,
			   current_frame_info.reg_size + first_offset);
		  if (doing_dwarf)
		    {
		      cfa_offset
			-= current_frame_info.reg_size + first_offset;
		      dwarf2out_def_cfa("",STACK_POINTER_REGNUM,cfa_offset);
		    }
		  first_offset = 0;
		  size_to_deallocate -= current_frame_info.reg_size;
		}
	    }

	  if (!(optimize_size && !TARGET_NO_MILLICODE_THUNK_SET))
	    {
	      if (current_frame_info.reg_size)
		arc_save_restore (file, sp_str,  0,
				  /* The zeroing of these two bits is unnecessary,
				     but leave this in for clarity.  */
				  current_frame_info.gmask
				  & ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK), "ld", -1, &first_offset);
	    }

        } /* ARCOMPACT */

      /* The rest of this function does the following:
         ARCtangent-A4: handle epilogue_delay, restore fp, sp, return
         ARCompact    : handle epilogue_delay, restore sp (phase-2), return
      */

      /* Keep track of how much of the stack pointer we've restored.
	 It makes the following a lot more readable. */
      if (TARGET_A4)
        {
          restored = 0;
          fp_restored_p = 0;
        }
      else
        {
	  size_to_deallocate += first_offset;
          restored = size - size_to_deallocate;
          fp_restored_p = 1;
        } /* if */

  
      if (TARGET_A4)
        {
          if (frame_pointer_needed)
            {
	      /* Try to restore the frame pointer in the delay slot.  We can't,
	         however, if any of these is true.  */
	      if (epilogue_delay != NULL_RTX
	          || !SMALL_INT (frame_size)
	          || pretend_size
	          || ARC_INTERRUPT_P (fn_type))
	        {
	          fprintf (file, "\tld.a %s,[%s,%d]\n",
                           fp_str, sp_str, frame_size);
	          restored += frame_size;
	          fp_restored_p = 1;
		  if(doing_dwarf)
		  {
		    if (current_function_calls_alloca || frame_pointer_needed)
		      dwarf2out_def_cfa("",FRAME_POINTER_REGNUM,cfa_offset);
		    else
		      {
			cfa_offset-=frame_size;
			dwarf2out_def_cfa("",STACK_POINTER_REGNUM,cfa_offset);
		      }
		  }
		}
            }
          else if (!SMALL_INT (size /* frame_size + pretend_size */)
	           || ARC_INTERRUPT_P (fn_type))
	    {
	       fprintf (file, "\tadd %s,%s,%d\n", sp_str, sp_str, frame_size);
	       restored += frame_size;
	       if(doing_dwarf)
	       {
		 if (current_function_calls_alloca || frame_pointer_needed)
		   dwarf2out_def_cfa("",FRAME_POINTER_REGNUM,cfa_offset);
		 else
		   {
		     cfa_offset-=frame_size;
		     dwarf2out_def_cfa("",STACK_POINTER_REGNUM,cfa_offset);
		   }
	       }
	    }
        } /* TARGET_A4 */

      /* These must be done before the return insn because the delay slot
	 does the final stack restore. */
      if (ARC_INTERRUPT_P (fn_type))
	{
	  if (epilogue_delay)
	    {
	      final_scan_insn (XEXP (epilogue_delay, 0), file, 1, 1, NULL);
	    }
        }

      if (!satisfies_constraint_C2a (GEN_INT (size - restored))
	  || (size > restored && epilogue_delay != NULL_RTX))
	{
	  fprintf (file, "\tadd_s %s,%s,%ld\n", sp_str, sp_str, size - restored);
	  restored = size;
	}
      /* Emit the return instruction.  */
      if (sibcall_epilogue == FALSE)
      {
	static int regs[4] = {
	  0, RETURN_ADDR_REGNUM, ILINK1_REGNUM, ILINK2_REGNUM
	};
	if (TARGET_A4)
	  fprintf (file, "\tj.d [%s]\n", reg_names[regs[fn_type]]);
	else if (epilogue_delay
		 && (get_attr_iscompact (XEXP (epilogue_delay, 0))
		     == ISCOMPACT_TRUE))
	  fprintf (file, "\tj.d [%s]\n",reg_names[regs[fn_type]]);
	else if (epilogue_delay != NULL_RTX || restored < size)
	  fprintf (file, "\tj_s.d [%s]\n",reg_names[regs[fn_type]]);
	else
	  fprintf (file, "\tj_s [%s]\n",reg_names[regs[fn_type]]);
      }

      /* If the only register saved is the return address, we need a
	 nop, unless we have an instruction to put into it.  Otherwise
	 we don't since reloading multiple registers doesn't reference
	 the register being loaded.  */

      if (TARGET_A4 && ARC_INTERRUPT_P (fn_type))
        {
          gcc_assert (!frame_pointer_needed || fp_restored_p);

          gcc_assert (restored >= size);
          fprintf (file, "\tadd %s,%s,16\n", sp_str, sp_str);
        }
      else if (epilogue_delay != NULL_RTX)
	{
	  gcc_assert (!frame_pointer_needed || fp_restored_p);

	  gcc_assert (restored >= size);
	  final_scan_insn (XEXP (epilogue_delay, 0), file, 1, 1, NULL);
	}
      else if (TARGET_A4 && frame_pointer_needed && !fp_restored_p)
	{
	  gcc_assert (SMALL_INT (frame_size));
          /* fp would have been restored earlier, if pretend_size */
          gcc_assert (pretend_size == 0);

	  fprintf (file, "\tld.a %s,[%s,%d]\n", fp_str, sp_str, frame_size);
	  if(doing_dwarf)
	    {
	      if (current_function_calls_alloca || frame_pointer_needed)
		  dwarf2out_def_cfa("",FRAME_POINTER_REGNUM,cfa_offset);
	      else
		{
		  cfa_offset-=frame_size;
		  dwarf2out_def_cfa("",STACK_POINTER_REGNUM,cfa_offset);
		}
	    }
	}
      else if (restored < size)
	{
	  /* For ARCtangent-A4:
	       The (size - restored) should always be SMALL_INT (9-bit sign
	       immediate). If it is long immediate it must be handled before
	       "j.d" insn.

	     For ARCompact:
	       The (size - restored) should always be SIGNED_INT12 (12-bit sign
	       immediate). */

	  /* Generate 16-bit instruction in delay slot of j_s only if 
	     optimizing for size. This is because a 16-bit instruction in the 
	     delay slot of a 16-bit jump introduces a delay */
	  if ((optimize_size || sibcall_epilogue) && (size - restored) < 128)
	    fprintf (file, "\tadd_s %s,%s,%ld\n", sp_str, sp_str,
	             size - restored);
	  else if (SIGNED_INT12 (size - restored))
	    fprintf (file, "\tadd %s,%s,%ld\n", sp_str, sp_str,
	             size - restored);
	  else
	    {
	      rtx operands[3];

	      operands[0] = operands[1] = stack_pointer_rtx;
	      operands[2] = GEN_INT (size - restored);
	      gcc_assert (!TARGET_A4 || SMALL_INT (size - restored));
	      gcc_assert (!TARGET_ARCOMPACT
			  || satisfies_constraint_C2a (operands[2]));

	      arc_output_addsi (operands, "");
	    }
	  if(doing_dwarf)
	    {
	      if (current_function_calls_alloca || frame_pointer_needed)
		  dwarf2out_def_cfa("",FRAME_POINTER_REGNUM,cfa_offset);
	      else
		{
		  cfa_offset-=(size - restored);
		  dwarf2out_def_cfa("",STACK_POINTER_REGNUM,cfa_offset);
		}
	    }
	}
      else if (!TARGET_ARCOMPACT)
	fprintf (file, "\tnop\n");
    }

  /* Reset state info for each function.  */
  current_frame_info = zero_frame_info;
  arc_compute_function_type (NULL_TREE);
}

/* Set up the stack and frame pointer (if desired) for the function.  */

/* Define the number of delay slots needed for the function epilogue.

   Interrupt handlers can't have any epilogue delay slots (it's always needed
   for something else, I think).  For normal functions, we have to worry about
   using call-saved regs as they'll be restored before the delay slot insn.
   Functions with non-empty frames already have enough choices for the epilogue
   delay slot so for now we only consider functions with empty frames.  */

int
arc_delay_slots_for_epilogue (void)
{
  if (arc_compute_function_type (current_function_decl) != ARC_FUNCTION_NORMAL)
    return 0;
  if (!current_frame_info.initialized)
    (void) arc_compute_frame_size (get_frame_size ());
  if (current_frame_info.total_size == 0)
    return 1;
  return 0;
}

/* Return true if TRIAL is a valid insn for the epilogue delay slot.
   Any single length instruction which doesn't reference the stack or frame
   pointer or any call-saved register is OK.  SLOT will always be 0.  */

int
arc_eligible_for_epilogue_delay (rtx trial,int slot)
{
  int trial_length = get_attr_length (trial);

  gcc_assert (slot == 0);

  if ( ( (trial_length == 4) || (trial_length == 2) )
      /* If registers where saved, presumably there's more than enough
	 possibilities for the delay slot.  The alternative is something
	 more complicated (of course, if we expanded the epilogue as rtl
	 this problem would go away).  */
      /* ??? Note that this will always be true since only functions with
	 empty frames have epilogue delay slots.  See
	 arc_delay_slots_for_epilogue.  */
      && current_frame_info.gmask == 0
      && ! reg_mentioned_p (stack_pointer_rtx, PATTERN (trial))
      && ! reg_mentioned_p (frame_pointer_rtx, PATTERN (trial)))
    return 1;
  return 0;
}


/* PIC */

/* Emit special PIC prologues and epilogues.  */
/* If the function has any GOTOFF relocations, then the GOTBASE
 * register has to be setup in the prologue 
 * The instruction needed at the function start for setting up the
 * GOTBASE register is
 *    add rdest, pc, 
 * ----------------------------------------------------------
 * The rtl to be emitted for this should be:
 *   set ( reg basereg) 
 *       ( plus ( reg pc) 
 *              ( const (unspec (symref _DYNAMIC) 3))) 
 * ----------------------------------------------------------
 */
/* Can be used when rtl pro/epilog comes in. 
   Unused till then */
void
arc_finalize_pic (void)
{
  rtx new;
  rtx baseptr_rtx = gen_rtx_REG (Pmode, PIC_OFFSET_TABLE_REGNUM);
  rtx pcrtx = gen_rtx_REG (Pmode, PROGRAM_COUNTER_REGNO);

  if (current_function_uses_pic_offset_table == 0)
    return;

  gcc_assert (flag_pic != 0);
  
  new = gen_rtx_SYMBOL_REF (Pmode, "_DYNAMIC");
  new = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, new), ARC_UNSPEC_GOT);
  new = gen_rtx_CONST (VOIDmode, new);
  new = gen_rtx_PLUS (Pmode, pcrtx, new);
  
  new = gen_move_insn (baseptr_rtx, new);

  /* emit this insn at the start of the function */
  emit_insn_after (gen_rtx_USE (VOIDmode, pic_offset_table_rtx), get_insns());
  emit_insn_after (new, get_insns ());

  return;
}

/* Output the assembler code for doing a shift.
   We go to a bit of trouble to generate efficient code as the ARC only has
   single bit shifts.  This is taken from the h8300 port.  We only have one
   mode of shifting and can't access individual bytes like the h8300 can, so
   this is greatly simplified (at the expense of not generating hyper-
   efficient code).

   This function is not used if the variable shift insns are present.  */

/* ??? We assume the output operand is the same as operand 1.
   This can be optimized (deleted) in the case of 1 bit shifts.  */
/* ??? We use the loop register here.  We don't use it elsewhere (yet) and
   using it here will give us a chance to play with it.  */

const char *
output_shift (rtx *operands)
{
  /*  static int loopend_lab;*/
  rtx shift = operands[3];
  enum machine_mode mode = GET_MODE (shift);
  enum rtx_code code = GET_CODE (shift);
  const char *shift_one;

  gcc_assert (mode == SImode);

  switch (code)
    {
    case ASHIFT:   shift_one = "asl %0,%0"; break;
    case ASHIFTRT: shift_one = "asr %0,%0"; break;
    case LSHIFTRT: shift_one = "lsr %0,%0"; break;
    default:       gcc_unreachable ();
    }

  if (GET_CODE (operands[2]) != CONST_INT)
    {
      output_asm_insn ("and.f %2, %2, 0x1f", operands);
      output_asm_insn ("mov lp_count,%2", operands);
      output_asm_insn ("bz 2f", operands);

      goto shiftloop;
    }
  else
    {
      int n = INTVAL (operands[2]);

      /* Only consider the lower 5 bits of the shift count */
      n = n & 0x1f;

      /* If the count is negative, take only lower 5 bits.  */
      /* FIXME: No longer needed */
      if (n < 0)
	n = n & 0x1f;

      /* If the count is too big, truncate it.
         ANSI says shifts of GET_MODE_BITSIZE are undefined - we choose to
	 do the intuitive thing.  */
      else if (n > GET_MODE_BITSIZE (mode))
	n = GET_MODE_BITSIZE (mode);

      /* First see if we can do them inline.  */
      if (n <= 8)
	{
	  while (--n >= 0)
	    output_asm_insn (shift_one, operands);
	}
      /* See if we can use a rotate/and.  */
      else if (n == BITS_PER_WORD - 1)
	{
	  switch (code)
	    {
	    case ASHIFT :
	      output_asm_insn ("and %0,%0,1\n\tror %0,%0", operands);
	      break;
	    case ASHIFTRT :
	      /* The ARC doesn't have a rol insn.  Use something else.  */
	      output_asm_insn ("asl.f 0,%0\n\tsbc %0,0,0", operands);
	      break;
	    case LSHIFTRT :
	      /* The ARC doesn't have a rol insn.  Use something else.  */
	      output_asm_insn ("asl.f 0,%0\n\tadc %0,0,0", operands);
	      break;
            default:
              break;
	    }
	}
      /* Must loop.  */
      else
	{
	  char buf[100];

	  sprintf (buf, "mov lp_count,%ld", INTVAL (operands[2]) & 0x1f );
	  output_asm_insn (buf, operands);

	shiftloop:
	    {
	      if (flag_pic)
		sprintf (buf, "lr %%4,[status]\n\tadd %%4,%%4,6\t%s single insn loop start",
			 ASM_COMMENT_START);
	      else
		sprintf (buf, "mov %%4,%%%%st(1f)\t%s (single insn loop start) >> 2",
			 ASM_COMMENT_START);
	      output_asm_insn (buf, operands);
	      output_asm_insn ("sr %4,[lp_start]", operands);
	      output_asm_insn ("add %4,%4,1", operands);
	      output_asm_insn ("sr %4,[lp_end]", operands);
	      output_asm_insn ("nop\n\tnop", operands);
	      if (flag_pic)
		asm_fprintf (asm_out_file, "\t%s single insn loop\n",
			     ASM_COMMENT_START);
	      else
		asm_fprintf (asm_out_file, "1:\t%s single insn loop\n",
			     ASM_COMMENT_START);
	      output_asm_insn (shift_one, operands);
	      fprintf (asm_out_file, "2:\t%s end single insn loop\n",
		       ASM_COMMENT_START);
	    }
	}
    }

  return "";
}

/* Nested function support.  */

/* Directly store VALUE at BASE plus OFFSET.  */
static void
emit_store_direct (rtx base, int offset, int value)
{
  emit_insn (gen_store_direct (gen_rtx_MEM (SImode,
					    plus_constant (base, offset)),
                               force_reg (SImode,
					  gen_int_mode (value, SImode))));
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */
/* With potentially multiple shared objects loaded, and multiple stacks
   present for multiple thereds where trampolines might reside, a simple
   range check will likely not suffice for the profiler to tell if a callee
   is a trampoline.  We a speedier check by making the trampoline start at
   an address that is not 4-byte aligned.
   A trampoline looks like this:

   nop_s	     0x78e0
entry:
   ld_s r12,[pcl,12] 0xd403
   ld   r11,[pcl,12] 0x170c 700b
   j_s [r12]         0x7c00
   nop_s	     0x78e0

   The fastest trampoline to execute for trampolines within +-8KB of CTX
   would be:
   add2 r11,pcl,s12
   j [limm]           0x20200f80 limm
   and that would also be faster to write to the stack by computing the offset
   from CTX to TRAMP at compile time.  However, it would really be better to
   get rid of the high cost of cache invalidation when generating trampolines,
   which requires that the code part of trampolines stays constant, and
   additionally either
   - making sure that no executable code but trampolines is on the stack,
     no icache entries linger for the area of the stack from when before the
     stack was allocated, and allocating trampolines in trampoline-only
     cache lines
  or
   - allocate trampolines fram a special pool of pre-allocated trampolines.  */


void
arc_initialize_trampoline (rtx tramp ATTRIBUTE_UNUSED,
			   rtx fnaddr ATTRIBUTE_UNUSED,
			   rtx cxt ATTRIBUTE_UNUSED)
{
  emit_store_direct (tramp, 0, TARGET_BIG_ENDIAN ? 0x78e0d403 : 0xd40378e0);
  emit_store_direct (tramp, 4, TARGET_BIG_ENDIAN ? 0x170c700b : 0x700b170c);
  emit_store_direct (tramp, 8, TARGET_BIG_ENDIAN ? 0x7c0078e0 : 0x78e07c00);
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 12)), fnaddr);
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 16)), cxt);
  emit_insn (gen_flush_icache (validize_mem (gen_rtx_MEM (SImode, tramp))));
}

/* Set the cpu type and print out other fancy things,
   at the top of the file.  */

void
arc_asm_file_start (FILE *file)
{
  fprintf (file, "\t.cpu %s\n", arc_cpu_string);
}


int
arc_cond_exec_p (void)
{
  return arc_ccfsm_state == 3 || arc_ccfsm_state == 4;
}

/* This is set briefly to 1 when we output a ".as" address modifer, and then
   reset when we output the scaled address.  */
static int output_scaled = 0;

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

void
arc_print_operand (FILE *file,rtx x,int code)
{
  switch (code)
    {
    case 'Z':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "%d",exact_log2(INTVAL (x) + 1) - 1 );
      else
	output_operand_lossage ("invalid operand to %%Z code");
      
      return;

    case 'z':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "%d",exact_log2(INTVAL (x)) );
      else
	output_operand_lossage ("invalid operand to %%z code");
      
      return;

    case 'M':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "%d",exact_log2(~INTVAL (x)) );
      else
	output_operand_lossage ("invalid operand to %%M code");
      
      return;

    case '#' :
      /* Conditional branches.  For now these are equivalent.  */
    case '*' :
      /* Unconditional branches.  Output the appropriate delay slot suffix.  */
      if (final_sequence && XVECLEN (final_sequence, 0) != 1)
	{
	  rtx jump = XVECEXP (final_sequence, 0, 0);
	  rtx delay = XVECEXP (final_sequence, 0, 1);
	  if (INSN_ANNULLED_BRANCH_P (jump))
	    fputs (INSN_FROM_TARGET_P (delay) ? 
                   ((arc_cpu == PROCESSOR_A4) ? ".jd" : ".d") : ".nd", file);
	  else
	    fputs (".d", file);
	}
      return;
    case '?' : /* with leading "." */
    case '!' : /* without leading "." */
      /* This insn can be conditionally executed.  See if the ccfsm machinery
	 says it should be conditionalized.  */
      if (arc_ccfsm_state == 3 || arc_ccfsm_state == 4)
	{
	  /* Is this insn in a delay slot?  */
	  if (final_sequence && XVECLEN (final_sequence, 0) == 2)
	    {
	      rtx jump = XVECEXP (final_sequence, 0, 0);
	      rtx insn = XVECEXP (final_sequence, 0, 1);

	      /* If the insn is annulled and is from the target path, we need
		 to inverse the condition test.  */
	      if (INSN_ANNULLED_BRANCH_P (jump))
		{
		  if (INSN_FROM_TARGET_P (insn))
		    fprintf (file, "%s%s",
			     code == '?' ? "." : "",
			     arc_condition_codes[ARC_INVERSE_CONDITION_CODE (arc_ccfsm_current_cc)]);
		  else
		    fprintf (file, "%s%s",
			     code == '?' ? "." : "",
			     arc_condition_codes[arc_ccfsm_current_cc]);
		}
		/* This insn is executed for either path, so don't
		   conditionalize it at all.  */
		/* nothing to do */
	      
	    }
	  else
	    {
	      /* This insn isn't in a delay slot.  */
	      fprintf (file, "%s%s",
		       code == '?' ? "." : "",
		       arc_condition_codes[arc_ccfsm_current_cc]);
	    }
	}
      return;
    case 'd' :
      fputs (arc_condition_codes[get_arc_condition_code (x)], file);
      return;
    case 'D' :
      fputs (arc_condition_codes[ARC_INVERSE_CONDITION_CODE
				 (get_arc_condition_code (x))],
	     file);
      return;
    case 'R' :
      /* Write second word of DImode or DFmode reference,
	 register or memory.  */
      if (GET_CODE (x) == REG)
	fputs (reg_names[REGNO (x)+1], file);
      else if (GET_CODE (x) == MEM)
	{
	  fputc ('[', file);

	  /* Handle possible auto-increment.  For PRE_INC / PRE_DEC /
	    PRE_MODIFY, we will have handled the first word already;
	    For POST_INC / POST_DEC / POST_MODIFY, the access to the
	    first word will be done later.  In either case, the access
	    to the first word will do the modify, and we only have
	    to add an offset of four here.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC
	      || GET_CODE (XEXP (x, 0)) == PRE_MODIFY
	      || GET_CODE (XEXP (x, 0)) == POST_INC
	      || GET_CODE (XEXP (x, 0)) == POST_DEC
	      || GET_CODE (XEXP (x, 0)) == POST_MODIFY)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 4));
	  else if (output_scaled)
	    {
	      rtx addr = XEXP (x, 0);
	      int size = GET_MODE_SIZE (GET_MODE (x));

	      output_address (plus_constant (XEXP (addr, 0),
					     ((INTVAL (XEXP (addr, 1)) + 4)
					      >> (size == 2 ? 1 : 2))));
	      output_scaled = 0;
	    }
	  else
	    output_address (plus_constant (XEXP (x, 0), 4));
	  fputc (']', file);
	}
      else
	output_operand_lossage ("invalid operand to %%R code");
      return;
    case 'S' :
	if (GET_CODE (x) == CONST
	    && GET_CODE( XEXP( XEXP (x,0),0)) == SYMBOL_REF
	    && GET_CODE (XEXP( XEXP (x,0),1)) == CONST_INT
	    && GET_CODE (XEXP (x,0)) == PLUS)
	{
	    if (TARGET_A4 && ARC_FUNCTION_NAME_PREFIX_P (* (XSTR (XEXP( XEXP (x,0),0), 0))))
	    {
		error ("Function address arithmetic is not supported.\n");
		return;
	    }
	}
	
	else if (symbolic_reference_mentioned_p(x))
	{
	    if(TARGET_A4  && ARC_FUNCTION_NAME_PREFIX_P (* (XSTR (x, 0))))
	    {
	      fprintf (file, "%%st(");
		output_addr_const (file, x);
		fprintf (file, ")");
		return;
	    }
	    else if (TARGET_A4 && GET_CODE (x) == LABEL_REF)
	    {
	      fprintf (file, "%%st(");
		output_addr_const (file, x);
		fprintf (file, ")");
		return;
	    }
	}
	
	else if (GET_CODE (x) == LABEL_REF)
	{
	    if (TARGET_A4)
	    {
		fprintf (file, "%%st(");
		output_addr_const (file, x);
		fprintf (file, ")");
		return;
	    }
	}
	break;
    case 'B' /* Branch - must not use sda references.  */ :
      if (CONSTANT_P (x))
	{
          output_addr_const (file, x); 
	  return;
	} 
      break;
    case 'H' :
    case 'L' :
      if (GET_CODE (x) == REG)
	{
	  /* L = least significant word, H = most significant word */
	  if ((WORDS_BIG_ENDIAN != 0) ^ (code == 'L'))
	    fputs (reg_names[REGNO (x)], file);
	  else
	    fputs (reg_names[REGNO (x)+1], file);
	}
      else if (GET_CODE (x) == CONST_INT
	       || GET_CODE (x) == CONST_DOUBLE)
	{
	  rtx first, second;

	  split_double (x, &first, &second);

	  if((WORDS_BIG_ENDIAN) == 0)
	      fprintf (file, "0x%08lx",
		       code == 'L' ? INTVAL (first) : INTVAL (second));
	  else
	      fprintf (file, "0x%08lx",
		       code == 'L' ? INTVAL (second) : INTVAL (first));
	      
	  
	  }
      else
	output_operand_lossage ("invalid operand to %%H/%%L code");
      return;
    case 'A' :
      {
	char str[30];

	gcc_assert (GET_CODE (x) == CONST_DOUBLE
		    && GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT);

	real_to_decimal (str, CONST_DOUBLE_REAL_VALUE (x), sizeof (str), 0, 1);
	fprintf (file, "%s", str);
	return;
      }
    case 'U' :
      /* Output a load/store with update indicator if appropriate.  */
      if (GET_CODE (x) == MEM)
	{
	  rtx addr = XEXP (x, 0);
	  switch (GET_CODE (addr))
	    {
	    case PRE_INC: case PRE_DEC: case PRE_MODIFY:
	      fputs (".a", file); break;
	    case POST_INC: case POST_DEC: case POST_MODIFY:
	      fputs (".ab", file); break;
	    case PLUS:
	      /* Can we use a scaled offset?  */
	      if (CONST_INT_P (XEXP (addr, 1))
		  && GET_MODE_SIZE (GET_MODE (x)) > 1
		  && (!(INTVAL (XEXP (addr, 1))
			& (GET_MODE_SIZE (GET_MODE (x)) - 1) & 3))
		  /* Does it make a difference?  */
		  && !SMALL_INT_RANGE(INTVAL (XEXP (addr, 1)),
				      GET_MODE_SIZE (GET_MODE (x)) - 2, 0))
		{
		  fputs (".as", file);
		  output_scaled = 1;
		}
	      /* Are we using a scaled index?  */
	      else if (GET_CODE (XEXP (addr, 0)) == MULT)
		fputs (".as", file);
	      break;
	    case REG:
	      break;
	    default:
	      gcc_assert (CONSTANT_P (addr)); break;
	    }
	}
      else
	output_operand_lossage ("invalid operand to %%U code");
      return;
    case 'V' :
      /* Output cache bypass indicator for a load/store insn.  Volatile memory
	 refs are defined to use the cache bypass mechanism.  */
      if (GET_CODE (x) == MEM)
	{
	  if (MEM_VOLATILE_P (x) && !TARGET_VOLATILE_CACHE_SET )
	    fputs (".di", file);
	}
      else
	output_operand_lossage ("invalid operand to %%V code");
      return;
      /* plt code */
    case 'P':
    case 0 :
      /* Do nothing special.  */
      break;
    case 'F':
      fputs (reg_names[REGNO (x)]+1, file);
      return;
    case '^':
	/* This punctuation character is needed because label references are
	printed in the output template using %l. This is a front end
	character, and when we want to emit a '@' before it, we have to use
	this '^'. */

	fputc('@',file);
	return;
    case 'O':
      /* Output an operator.  */
      switch (GET_CODE (x))
	{
	case PLUS:	fputs ("add", file); return;
	case SS_PLUS:	fputs ("adds", file); return;
	case AND:	fputs ("and", file); return;
	case IOR:	fputs ("or", file); return;
	case XOR:	fputs ("xor", file); return;
	case MINUS:	fputs ("sub", file); return;
	case SS_MINUS:	fputs ("subs", file); return;
	case ASHIFT:	fputs ("asl", file); return;
	case ASHIFTRT:	fputs ("asr", file); return;
	case LSHIFTRT:	fputs ("lsr", file); return;
	case ROTATERT:	fputs ("ror", file); return;
	case MULT:	fputs ("mpy", file); return;
	case ABS:	fputs ("abs", file); return; /* unconditional */
	case NEG:	fputs ("neg", file); return;
	case SS_NEG:	fputs ("negs", file); return;
	case NOT:	fputs ("not", file); return; /* unconditional */
	case ZERO_EXTEND:
	  fputs ("ext", file); /* bmsk allows predication.  */
	  goto size_suffix;
	case SIGN_EXTEND: /* unconditional */
	  fputs ("sex", file);
	size_suffix:
	  switch (GET_MODE (XEXP (x, 0)))
	    {
	    case QImode: fputs ("b", file); return;
	    case HImode: fputs ("w", file); return;
	    default: break;
	    }
	  break;
	case SS_TRUNCATE:
	  if (GET_MODE (x) != HImode)
	    break;
	  fputs ("sat16", file);
	default: break;
	}
      output_operand_lossage ("invalid operand to %%O code"); return;
    default :
      /* Unknown flag.  */
      output_operand_lossage ("invalid operand output code");
    }

  switch (GET_CODE (x))
    {
    case REG :
      fputs (reg_names[REGNO (x)], file);
      break;
    case MEM :
      {
	rtx addr = XEXP (x, 0);
	int size = GET_MODE_SIZE (GET_MODE (x));

	fputc ('[', file);

	switch (GET_CODE (addr))
	  {
	  case PRE_INC: case POST_INC:
	    output_address (plus_constant (XEXP (addr, 0), size)); break;
	  case PRE_DEC: case POST_DEC:
	    output_address (plus_constant (XEXP (addr, 0), -size)); break;
	  case PRE_MODIFY: case POST_MODIFY:
	    output_address (XEXP (addr, 1)); break;
	  case PLUS:
	    if (output_scaled)
	      {
		output_address (plus_constant (XEXP (addr, 0),
					       (INTVAL (XEXP (addr, 1))
						>> (size == 2 ? 1 : 2))));
		output_scaled = 0;
	      }
	    else
	      output_address (addr);
	    break;
	  default:
	    if (flag_pic && CONSTANT_ADDRESS_P (addr))
	      arc_output_pic_addr_const (file, addr, code);
	    else
	      output_address (addr);
	    break;
	  }
	fputc (']', file);
	break;
      }
    case CONST_DOUBLE :
      /* We handle SFmode constants here as output_addr_const doesn't.  */
      if (GET_MODE (x) == SFmode)
	{
	  REAL_VALUE_TYPE d;
	  long l;

	  REAL_VALUE_FROM_CONST_DOUBLE (d, x);
	  REAL_VALUE_TO_TARGET_SINGLE (d, l);
	  fprintf (file, "0x%08lx", l);
	  break;
	}
      /* Fall through.  Let output_addr_const deal with it.  */
    default :
      if (flag_pic)
      	arc_output_pic_addr_const (file, x, code);
      else
	{
	  /* FIXME: Dirty way to handle @var@sda+const. Shd be handled
	     with asm_output_symbol_ref */
	  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS)
	    {
	      x = XEXP (x, 0);
	      output_addr_const (file, XEXP (x, 0));
	      if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF && SYMBOL_REF_SMALL_P (XEXP (x, 0))) 
		fprintf (file, "@sda");		
	      
	      if (GET_CODE (XEXP (x, 1)) != CONST_INT
		  || INTVAL (XEXP (x, 1)) >= 0)
		fprintf (file, "+");
	      output_addr_const (file, XEXP (x, 1));
	    }
	  else
	    output_addr_const (file, x);
	}
      if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_SMALL_P (x))
	fprintf (file, "@sda");
      break;
    }
}

/* Print a memory address as an operand to reference that memory location.  */

void
arc_print_operand_address (FILE *file , rtx addr)
{
  register rtx base, index = 0;

  switch (GET_CODE (addr))
    {
    case REG :
      fputs (reg_names[REGNO (addr)], file);
      break;
    case SYMBOL_REF :
      if (TARGET_A4 && ARC_FUNCTION_NAME_PREFIX_P (* (XSTR (addr, 0))))
	{
	  fprintf (file, "%%st(");
	  output_addr_const (file, addr);
	  fprintf (file, ")");
	}
      else
	{
	  output_addr_const (file, addr);
	  if (SYMBOL_REF_SMALL_P (addr))
	    fprintf (file, "@sda");
	}
      break;
    case PLUS :
      if (GET_CODE (XEXP (addr, 0)) == MULT)
	index = XEXP (XEXP (addr, 0), 0), base = XEXP (addr, 1);
      else if (CONST_INT_P (XEXP (addr, 0)))
	index = XEXP (addr, 0), base = XEXP (addr, 1);
      else
	base = XEXP (addr, 0), index = XEXP (addr, 1);

      gcc_assert (OBJECT_P (base));
      arc_print_operand_address (file, base);
      if (CONSTANT_P (base) && CONST_INT_P (index))
	fputc ('+', file);
      else
	fputc (',', file);
      gcc_assert (OBJECT_P (index));
      arc_print_operand_address (file, index);
      break;
    case CONST:
      {
	rtx c = XEXP (addr, 0);

	gcc_assert (GET_CODE (XEXP (c, 0)) == SYMBOL_REF);
	gcc_assert (GET_CODE (XEXP (c, 1)) == CONST_INT);

	output_address(XEXP(addr,0));
	
	break;
      }
    case PRE_INC :
    case PRE_DEC :
      /* We shouldn't get here as we've lost the mode of the memory object
	 (which says how much to inc/dec by.  */
      gcc_unreachable ();
      break;
    default :
      if (flag_pic)
	arc_output_pic_addr_const (file, addr, 0);
      else
	output_addr_const (file, addr);
      break;
    }
}

/* Update compare/branch separation marker.  */

static void record_cc_ref (rtx insn)
{
  last_insn_set_cc_p = current_insn_set_cc_p;

  switch (get_attr_cond (insn))
    {

       	/* We have added the length of nops to the compare instruction, so
    that no offset calculations go wrong. Hence the length of the compare
    has increased from 4 to 8 in case of no mixed_code_enabled for a4,a5,a6,
    and from 4 to 6 in case of mixed_code_enabled for a4, a5, a6( We dont
    generate nops for a7). Hence we have added the extra checks for lengths
    8 and 6 below. No instructions that have length 8 or 6 can set the
    cc.(?? :) Even if she does, he should make current_insn_set_cc_p = 1,I think) */    
   
    case COND_SET :
    case COND_SET_ZN :
	if ((get_attr_length (insn) == 8 ) || ( get_attr_length (insn) == 6 && get_attr_iscompact (insn) ) || 
	    (get_attr_length (insn) == 12 ) || ( get_attr_length (insn) == 10 && get_attr_iscompact (insn) ) || 
	    (get_attr_length (insn) == 4) || (get_attr_length (insn) == 2))
	{
	     current_insn_set_cc_p = 1;
	}
	else
	    current_insn_set_cc_p = 0;
      break;
    default :
      current_insn_set_cc_p = 0;
      break;
    }
}

/* Called via note_stores.  */
static void
write_profile_sections (rtx dest ATTRIBUTE_UNUSED, rtx x, void *data)
{
  rtx *srcp, src;
  htab_t htab = data;
  rtx *slot;

  if (GET_CODE (x) != SET)
    return;
  srcp = &SET_SRC (x);
  if (MEM_P (*srcp))
    srcp = &XEXP (*srcp, 0);
  else if (MEM_P (SET_DEST (x)))
    srcp = &XEXP (SET_DEST (x), 0);
  src = *srcp;
  if (GET_CODE (src) != CONST)
    return;
  src = XEXP (src, 0);
  if (GET_CODE (src) != UNSPEC || XINT (src, 1) != UNSPEC_PROF)
    return;

  gcc_assert (XVECLEN (src, 0) == 3);
  if (!htab_elements (htab))
    {
      output_asm_insn (".section .__arc_profile_desc, \"a\"\n"
		       "\t.long %0 + 1\n",
		       &XVECEXP (src, 0, 0));
    }
  slot = (rtx *) htab_find_slot (htab, src, INSERT);
  if (*slot == HTAB_EMPTY_ENTRY)
    {
      static int count_nr;
      char buf[24];
      rtx count;

      *slot = src;
      sprintf (buf, "__prof_count%d", count_nr++);
      count = gen_rtx_SYMBOL_REF (Pmode, xstrdup (buf));
      XVECEXP (src, 0, 2) = count;
      output_asm_insn (".section\t.__arc_profile_desc, \"a\"\n"
		       "\t.long\t%1\n"
		       "\t.section\t.__arc_profile_counters, \"aw\"\n"
		       "\t.type\t%2, @object\n"
		       "\t.size\t%2, 4\n"
		       "%2:\t.zero 4",
		       &XVECEXP (src, 0, 0));
      *srcp = count;
    }
  else
    *srcp = XVECEXP (*slot, 0, 2);
}

static hashval_t
unspec_prof_hash (const void *x)
{
  const rtx u = (rtx) x;
  rtx s1 = XVECEXP (u, 0, 1);

  return (htab_hash_string (XSTR (XVECEXP (u, 0, 0), 0))
	  ^ (s1->code == SYMBOL_REF ? htab_hash_string (XSTR (s1, 0)) : 0));
}

static int
unspec_prof_htab_eq (const void *x, const void *y)
{
  const rtx u0 = (rtx) x;
  const rtx u1 = (rtx) y;
  rtx s01 = XVECEXP (u0, 0, 1);
  rtx s11 = XVECEXP (u1, 0, 1);

  return (!strcmp (XSTR (XVECEXP (u0, 0, 0), 0),
		   XSTR (XVECEXP (u1, 0, 0), 0))
	  && rtx_equal_p (s01, s11));
}

/* Conditional execution support.

   This is based on the ARM port but for now is much simpler.

   A finite state machine takes care of noticing whether or not instructions
   can be conditionally executed, and thus decrease execution time and code
   size by deleting branch instructions.  The fsm is controlled by
   final_prescan_insn, and controls the actions of PRINT_OPERAND.  The patterns
   in the .md file for the branch insns also have a hand in this.  */

/* The state of the fsm controlling condition codes are:
   0: normal, do nothing special
   1: don't output this insn
   2: don't output this insn
   3: make insns conditional
   4: make insns conditional

   State transitions (state->state by whom, under what condition):
   0 -> 1 final_prescan_insn, if insn is conditional branch
   0 -> 2 final_prescan_insn, if the `target' is an unconditional branch
   1 -> 3 branch patterns, after having not output the conditional branch
   2 -> 4 branch patterns, after having not output the conditional branch
   3 -> 0 ASM_OUTPUT_INTERNAL_LABEL, if the `target' label is reached
          (the target label has CODE_LABEL_NUMBER equal to
	  arc_ccfsm_target_label).
   4 -> 0 final_prescan_insn, if `target' unconditional branch is reached

   If the jump clobbers the conditions then we use states 2 and 4.

   A similar thing can be done with conditional return insns.

   We also handle separating branches from sets of the condition code.
   This is done here because knowledge of the ccfsm state is required,
   we may not be outputting the branch.  */

/* ??? This function is to be re-checked */
void
arc_final_prescan_insn (rtx insn,rtx *opvec ATTRIBUTE_UNUSED,
			int noperands ATTRIBUTE_UNUSED)
{
  /* BODY will hold the body of INSN.  */
  register rtx body = PATTERN (insn);

  /* This will be 1 if trying to repeat the trick (ie: do the `else' part of
     an if/then/else), and things need to be reversed.  */
  int reverse = 0;

  /* If we start with a return insn, we only succeed if we find another one. */
  int seeking_return = 0;
  
  /* START_INSN will hold the insn from where we start looking.  This is the
     first insn after the following code_label if REVERSE is true.  */
  rtx start_insn = insn;

  /* Type of the jump_insn. Brcc insns don't affect ccfsm changes, 
     since they don't rely on a cmp preceding them */
  enum attr_type jump_insn_type;

  if (TARGET_DUMPISIZE)
    fprintf (asm_out_file, "\n! at %04x\n", INSN_ADDRESSES (INSN_UID (insn)));

  /* Update compare/branch separation marker.  */
  record_cc_ref (insn);

  /* Allow -mdebug-ccfsm to turn this off so we can see how well it does.
     We can't do this in macro FINAL_PRESCAN_INSN because its called from
     final_scan_insn which has `optimize' as a local.  */
  if (optimize < 2 || TARGET_NO_COND_EXEC)
    return;
  /* If in state 4, check if the target branch is reached, in order to
     change back to state 0.  */
  if (arc_ccfsm_state == 4)
    {
      if (insn == arc_ccfsm_target_insn)
	{
	  arc_ccfsm_target_insn = NULL;
	  arc_ccfsm_state = 0;
	}
      return;
    }

  /* If in state 3, it is possible to repeat the trick, if this insn is an
     unconditional branch to a label, and immediately following this branch
     is the previous target label which is only used once, and the label this
     branch jumps to is not too far off.  Or in other words "we've done the
     `then' part, see if we can do the `else' part."  */
  if (arc_ccfsm_state == 3)
    {
      if (simplejump_p (insn))
	{
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    {
	      /* ??? Isn't this always a barrier?  */
	      start_insn = next_nonnote_insn (start_insn);
	    }
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == arc_ccfsm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    reverse = TRUE;
	  else
	    return;
	}
      else if (GET_CODE (body) == RETURN)
        {
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == arc_ccfsm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    {
	      reverse = TRUE;
	      seeking_return = 1;
	    }
	  else
	    return;
        }
      else
	return;
    }

  if (GET_CODE (insn) != JUMP_INSN)
    return;

  if ((jump_insn_type = get_attr_type(insn)) == TYPE_BRCC 
      || jump_insn_type == TYPE_BRCC_NO_DELAY_SLOT)
      return;

  /* This jump might be paralleled with a clobber of the condition codes,
     the jump should always come first.  */
  if (GET_CODE (body) == PARALLEL && XVECLEN (body, 0) > 0)
    body = XVECEXP (body, 0, 0);

  if (reverse
      || (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
	  && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE))
    {
      int insns_skipped = 0, fail = FALSE, succeed = FALSE;
      /* Flag which part of the IF_THEN_ELSE is the LABEL_REF.  */
      int then_not_else = TRUE;
      /* Nonzero if next insn must be the target label.  */
      int next_must_be_target_label_p;
      rtx this_insn = start_insn, label = 0;

      /* Register the insn jumped to.  */
      if (reverse)
        {
	  if (!seeking_return)
	    label = XEXP (SET_SRC (body), 0);
        }
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == LABEL_REF)
	label = XEXP (XEXP (SET_SRC (body), 1), 0);
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == LABEL_REF)
	{
	  label = XEXP (XEXP (SET_SRC (body), 2), 0);
	  then_not_else = FALSE;
	}
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == RETURN)
	seeking_return = 1;
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == RETURN)
        {
	  seeking_return = 1;
	  then_not_else = FALSE;
        }
      else
	gcc_unreachable ();

      /* See how many insns this branch skips, and what kind of insns.  If all
	 insns are okay, and the label or unconditional branch to the same
	 label is not too far away, succeed.  */
      for (insns_skipped = 0, next_must_be_target_label_p = FALSE;
	   !fail && !succeed && insns_skipped < MAX_INSNS_SKIPPED;
	   insns_skipped++)
	{
	  rtx scanbody;

	  this_insn = next_nonnote_insn (this_insn);
	  if (!this_insn)
	    break;

	  if (next_must_be_target_label_p)
	    {
	      if (GET_CODE (this_insn) == BARRIER)
		continue;
	      if (GET_CODE (this_insn) == CODE_LABEL
		  && this_insn == label)
		{
		  arc_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;
	    }

	  scanbody = PATTERN (this_insn);

	  switch (GET_CODE (this_insn))
	    {
	    case CODE_LABEL:
	      /* Succeed if it is the target label, otherwise fail since
		 control falls in from somewhere else.  */
	      if (this_insn == label)
		{
		  arc_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case BARRIER:
	      /* Succeed if the following insn is the target label.
		 Otherwise fail.  
		 If return insns are used then the last insn in a function 
		 will be a barrier. */
	      next_must_be_target_label_p = TRUE;
	      break;

	    case CALL_INSN:
	      /* Can handle a call insn if there are no insns after it.
		 IE: The next "insn" is the target label.  We don't have to
		 worry about delay slots as such insns are SEQUENCE's inside
		 INSN's.  ??? It is possible to handle such insns though.  */
	      if (get_attr_cond (this_insn) == COND_CANUSE)
		next_must_be_target_label_p = TRUE;
	      else
		fail = TRUE;
	      break;

	    case JUMP_INSN:
      	      /* If this is an unconditional branch to the same label, succeed.
		 If it is to another label, do nothing.  If it is conditional,
		 fail.  */
	      /* ??? Probably, the test for the SET and the PC are unnecessary. */

	      if (GET_CODE (scanbody) == SET
		  && GET_CODE (SET_DEST (scanbody)) == PC)
		{
		  if (GET_CODE (SET_SRC (scanbody)) == LABEL_REF
		      && XEXP (SET_SRC (scanbody), 0) == label && !reverse)
		    {
		      arc_ccfsm_state = 2;
		      succeed = TRUE;
		    }
		  else if (GET_CODE (SET_SRC (scanbody)) == IF_THEN_ELSE)
		    fail = TRUE;
		  else if (get_attr_cond (this_insn) != COND_CANUSE)
		    fail = TRUE;
		}
	      else if (GET_CODE (scanbody) == RETURN
		       && seeking_return)
	        {
		  arc_ccfsm_state = 2;
		  succeed = TRUE;
	        }
	      else if (GET_CODE (scanbody) == PARALLEL)
	        {
		  if (get_attr_cond (this_insn) != COND_CANUSE)
		    fail = TRUE;
		}
	      break;

	    case INSN:
	      /* We can only do this with insns that can use the condition
		 codes (and don't set them).  */
	      if (GET_CODE (scanbody) == SET
		  || GET_CODE (scanbody) == PARALLEL)
		{
		  if (get_attr_cond (this_insn) != COND_CANUSE)
		    fail = TRUE;
		}
	      /* We can't handle other insns like sequences.  */
	      else
		fail = TRUE;
	      break;

	    default:
	      break;
	    }
	}

      if (succeed)
	{
	  if ((!seeking_return) && (arc_ccfsm_state == 1 || reverse))
	    arc_ccfsm_target_label = CODE_LABEL_NUMBER (label);
	  else if (seeking_return || arc_ccfsm_state == 2)
	    {
	      while (this_insn && GET_CODE (PATTERN (this_insn)) == USE)
	        {
		  this_insn = next_nonnote_insn (this_insn);

		  gcc_assert (!this_insn || 
			      (GET_CODE (this_insn) != BARRIER
			       && GET_CODE (this_insn) != CODE_LABEL));
	        }
	      if (!this_insn)
	        {
		  /* Oh dear! we ran off the end, give up.  */
		  extract_insn_cached (insn);
		  arc_ccfsm_state = 0;
		  arc_ccfsm_target_insn = NULL;
		  return;
	        }
	      arc_ccfsm_target_insn = this_insn;
	    }
	  else
	    gcc_unreachable ();

	  /* If REVERSE is true, ARM_CURRENT_CC needs to be inverted from
	     what it was.  */
	  if (!reverse)
	    arc_ccfsm_current_cc = get_arc_condition_code (XEXP (SET_SRC (body),
								 0));

	  if (reverse || then_not_else)
	    arc_ccfsm_current_cc = ARC_INVERSE_CONDITION_CODE (arc_ccfsm_current_cc);
	}

      /* Restore recog_operand.  Getting the attributes of other insns can
	 destroy this array, but final.c assumes that it remains intact
	 across this call; since the insn has been recognized already we
	 call insn_extract direct. */
     extract_insn_cached (insn);
    }
}

/* Record that we are currently outputting label NUM with prefix PREFIX.
   It it's the label we're looking for, reset the ccfsm machinery.

   Called from ASM_OUTPUT_INTERNAL_LABEL.  */

void
arc_ccfsm_at_label (const char *prefix,int num)
{
  if (arc_ccfsm_state == 3 && arc_ccfsm_target_label == num
      && !strcmp (prefix, "L"))
    {
      arc_ccfsm_state = 0;
      arc_ccfsm_target_insn = NULL_RTX;
    }
}

/* See if the current insn, which is a conditional branch, is to be
   deleted.  */

int
arc_ccfsm_branch_deleted_p (void)
{
  if (arc_ccfsm_state == 1 || arc_ccfsm_state == 2)
    return 1;
  return 0;
}

/* Record a branch isn't output because subsequent insns can be
   conditionalized.  */

void
arc_ccfsm_record_branch_deleted (void)
{
  /* Indicate we're conditionalizing insns now.  */
  arc_ccfsm_state += 2;

  /* If the next insn is a subroutine call, we still need a nop between the
     cc setter and user.  We need to undo the effect of calling record_cc_ref
     for the just deleted branch.  */
  current_insn_set_cc_p = last_insn_set_cc_p;
}

/* Define the offset between two registers, one to be eliminated, and
   the other its replacement, at the start of a routine.  */

int
arc_initial_elimination_offset (int from,int to)
{
  if (! current_frame_info.initialized)
     arc_compute_frame_size (get_frame_size ());

  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    {
      if (TARGET_A4)
        return 0;
      else
        return (current_frame_info.extra_size
                + current_frame_info.reg_size);
    }

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
        return (current_frame_info.total_size
                - current_frame_info.pretend_size);
    }

  if ((from == FRAME_POINTER_REGNUM) && (to == STACK_POINTER_REGNUM))
    {
      if (TARGET_A4)
        return (current_frame_info.total_size 
                - current_frame_info.pretend_size);
      else
        return (current_frame_info.total_size
                - (current_frame_info.pretend_size
                   + current_frame_info.extra_size
                   + current_frame_info.reg_size));
    }

  gcc_unreachable ();
}


/* Generate a bbit{0,1} insn for the current pattern  
 * bbit instructions are used as an optimized alternative to 
 * a sequence of bic,cmp and branch instructions
 * Similar to gen_bbit_insns(), with conditions reversed
 */
const char *
gen_bbit_bic_insns(rtx * operands)
{
  
  switch (INTVAL(operands[3]))
  {
    /*  bic r%0,imm%1,r%2
     *  cmp r%0,0<- the value we have switched on
     *  b{eq,ne} label%5 
     *  ||
     *	\/
     * bbit{0,1} r%1,log2(imm%2),label%5
     */
  case 0:
    if ( GET_CODE (operands[4]) == EQ ) {
      return "bbit1%# %1,%z2,%^%l5";
    }
    else if ( GET_CODE (operands[4]) == NE )
      return "bbit0%# %1,%z2,%^%l5";
    else
      gcc_unreachable();
    
    /*  bic r%0,imm%1,r%2
     *  cmp r%0,0<- the value we have switched on
     *  b{eq,ne} label%5 
     *  ||
     *	\/
     * bbit{0,1} r%1,log2(imm%2),label%5
     * the bne case does not make sense here as it gives too little 
     * information for us to generate an insn.
     * Such a case is therefore disallowed in the condition itself.
     * ( ref: valid_bbit_pattern_p )
     */
  case 1:
    if ( GET_CODE (operands[4]) == EQ )
      return "bbit0%# %1,%z2,%l5";
    else
      gcc_unreachable();

  default:
    gcc_unreachable();
  }
}



/* Generate a bbit{0,1} insn for the current pattern  
 * bbit instructions are used as an optimized alternative to 
 * a sequence of and,cmp and branch instructions   
 */
const char *
gen_bbit_insns(rtx * operands)
{
  
  switch (INTVAL(operands[3]))
  {
    /* and r%0,r%1,imm%2
     *  cmp r%0,0<- the value we have switched on
     *  b{eq,ne} label%5 
     *  ||
     *	\/
     * bbit{0,1} r%0,log2(imm%2),label%5
     */
  case 0:
    if ( GET_CODE (operands[4]) == EQ )
      return "bbit0%# %1,%z2,%^%l5";
    else if ( GET_CODE (operands[4]) == NE )
      return "bbit1%# %1,%z2,%^%l5";
    else
      gcc_unreachable();
    
    /* and r%0,r%1,imm%2
     *  cmp r%0,1<- the value we have switched on
     *  beq label%5 
     *  ||
     *	\/
     * bbit1 r%0,log2(imm%2),label%5
     * the bne case does not make sense here as it gives too little 
     * information for us to generate an insn.
     * Such a case is therefore disallowed in the condition itself.
     * ( ref: valid_bbit_pattern_p )
     */
  case 1:
    if ( GET_CODE (operands[4]) == EQ )
      return "bbit1%# %1,%z2,%l5";
    else
      gcc_unreachable();

  default:
    gcc_unreachable();
  }
}


/* Return the destination address of a branch.  */
int branch_dest (rtx branch)
{
  rtx dest = SET_SRC (PATTERN (branch));
  int dest_uid;

  if (GET_CODE (dest) == IF_THEN_ELSE)
    dest = XEXP (dest, XEXP (dest, 1) == pc_rtx ? 2 : 1);

  dest = XEXP (dest, 0);
  dest_uid = INSN_UID (dest);

  return INSN_ADDRESSES (dest_uid);
}


/* Predicate for judging if a pattern is valid for bbit generation 
 * The rtl pattern is:
 *       and r%0,r%1,imm%2
 *       cmp r%0, imm%3
 *       pc = (cmp cc, 0) ? label%5 : pc
 * The conditions required are:
 *      1. imm%2 shd be an exact power of 2
 *      2. imm%3 shd be 0 or 1
 *      3. the comparison operator should be either EQ or NE
 *      NOTE: imm%3 = 1 and comparion = NE is not valid
 */
int
valid_bbit_pattern_p (rtx * operands,rtx insn)
{
  int retval; 

  /* ret = (imm%2 == power of 2 */
  retval = !( (INTVAL(operands[2]) & (INTVAL(operands[2]) - 1)) );
  
  /* now check for the right combinations 
   * ( ref: comments in gen_bbit_insns above )
   */
  retval = retval && 
    (
     ( INTVAL(operands[3]) == 1 && GET_CODE (operands[4]) == EQ )
     || ( ( INTVAL(operands[3]) == 0) 
	  && ( GET_CODE (operands[4]) == EQ || GET_CODE (operands[4]) == NE))
     );

  retval = retval && SMALL_INT(branch_dest(insn)-INSN_ADDRESSES(INSN_UID(insn)));

  return retval;

}

/* Symbols in the text segment can be accessed without indirecting via the
   constant pool; it may take an extra binary operation, but this is still
   faster than indirecting via memory.  Don't do this when not optimizing,
   since we won't be calculating al of the offsets necessary to do this
   simplification.  */

/* On the ARC, function addresses are not the same as normal addresses.
   Branch to absolute address insns take an address that is right-shifted
   by 2.  We encode the fact that we have a function here, and then emit a
   special assembler op when outputting the address.
   The encoding involves adding an *_CALL_FLAG_CHAR to the symbol name 
   (depending on whether any of short_call/long_call attributes were specified
   in the function's declaration) and  unmangling the name at the time of 
   printing the symbol name.

   Also if the symbol is a local, then the machine specific
   SYMBOL_REF_FLAG is set in the rtx.This flag is later used to print
   the reference to local symbols as @GOTOFF references instead of
   @GOT references so that the symbol does not get a GOT entry unlike
   the global symbols.
   Also calls to local functions are relative and not through the
   Procedure Linkage Table. 
*/

static void arc_encode_section_info (tree decl, rtx rtl ATTRIBUTE_UNUSED,
				     int first ATTRIBUTE_UNUSED)
{
  tree attr, long_call_attr, short_call_attr;
  
  attr = TYPE_ATTRIBUTES (TREE_TYPE (decl));

  /* Check if it is a function, and whether it has the [long/short]_call 
     attribute specified */
  if ( TREE_CODE (decl) == FUNCTION_DECL )
    {
      long_call_attr = lookup_attribute ("long_call", attr);
      short_call_attr = lookup_attribute ("short_call", attr);
      
      if (long_call_attr != NULL_TREE)
	{
	  arc_encode_symbol (decl, LONG_CALL_FLAG_CHAR);
	}
      else if (short_call_attr != NULL_TREE)
	{
	  arc_encode_symbol (decl, SHORT_CALL_FLAG_CHAR);
	}
      else
	arc_encode_symbol (decl, SIMPLE_CALL_FLAG_CHAR);
    }

    if (flag_pic)
    {
      if ( !DECL_P(decl)
	  || targetm.binds_local_p (decl))
         SYMBOL_REF_FLAG (XEXP (rtl, 0)) = 1;
    }

    /* for sdata */
    default_encode_section_info (decl, rtl, first);

}

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

static void arc_internal_label (FILE *stream, const char *prefix, unsigned long labelno)
{
  arc_ccfsm_at_label (prefix, labelno);
  default_internal_label (stream, prefix, labelno);
}

/* Set the cpu type and print out other fancy things,
   at the top of the file.  */

static void arc_file_start (void)
{
  default_file_start ();
  fprintf (asm_out_file, "\t.cpu %s\n", arc_cpu_string);
}

static void arc_asm_file_end (void)
{
  /* Free the obstack */
  /*    obstack_free (&arc_local_obstack, NULL);*/
 
}
/* Cost functions.  */

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
arc_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED, int *total)
{
  switch (code)
    {
      /* Small integers are as cheap as registers.  4 byte values can
	 be fetched as immediate constants - let's give that the cost
	 of an extra insn.  */
    case CONST_INT:
      if (SMALL_INT (INTVAL (x)))
	{
	  *total = 0;
	  return true;
	}
      /* FALLTHRU */

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
          *total = COSTS_N_INSNS (1);
      return true;

    case CONST_DOUBLE:
      {
        rtx high, low;

	if (TARGET_DPFP) 
	  {
	    *total = COSTS_N_INSNS (1);
	    return true;
	  }
	/* FIXME: correct the order of high,low */
        split_double (x, &high, &low);
	*total = COSTS_N_INSNS (!SMALL_INT (INTVAL (high))
				+ !SMALL_INT (INTVAL (low)));
	return true;
      }

    /* Encourage synth_mult to find a synthetic multiply when reasonable.
       If we need more than 12 insns to do a multiply, then go out-of-line,
       since the call overhead will be < 10% of the cost of the multiply.  */
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if (TARGET_SHIFTER)
        *total = COSTS_N_INSNS (1);
      else if (GET_CODE (XEXP (x, 1)) != CONST_INT)
        *total = COSTS_N_INSNS (16);
      else
        *total = COSTS_N_INSNS (INTVAL (XEXP ((x), 1)));
      return false;

    case DIV:
    case UDIV:
      if (optimize_size)
	*total = COSTS_N_INSNS(1);
      else
	*total = COSTS_N_INSNS(30);
	return false;

    case MULT: 
      /* We do not want inlined multiply when optimizing 
	 for size */                                           
      if ((TARGET_DPFP && GET_MODE (x) == DFmode)
	  || optimize_size)
	*total = COSTS_N_INSNS (1);
      else
	*total= arc_multcost;
      return false;
    case PLUS:
      if (GET_CODE (XEXP (x, 0)) == MULT
	  && _2_4_8_operand (XEXP (XEXP (x, 0), 1), VOIDmode))
	{
	  *total += (rtx_cost (XEXP (x, 1), PLUS)
		     + rtx_cost (XEXP (XEXP (x, 0), 0), PLUS));
	  return true;
	}
      return false;
    case MINUS:
      if (GET_CODE (XEXP (x, 1)) == MULT
	  && _2_4_8_operand (XEXP (XEXP (x, 1), 1), VOIDmode))
	{
	  *total += (rtx_cost (XEXP (x, 0), PLUS)
		     + rtx_cost (XEXP (XEXP (x, 1), 0), PLUS));
	  return true;
	}
      return false;

    default:
     return false;
    }
}


void
  arc_va_start (tree valist, rtx nextarg)
  {
    /* See arc_setup_incoming_varargs for reasons for this oddity.  */
    if (current_function_args_info < 8
        && (current_function_args_info & 1))
      nextarg = plus_constant (nextarg, UNITS_PER_WORD);
  
    std_expand_builtin_va_start (valist, nextarg);
  }


rtx
arc_va_arg (tree valist, tree type)
  {
    rtx addr_rtx;
    tree addr, incr;
    tree type_ptr = build_pointer_type (type);
  
#if 0
    /* All aggregates are passed by reference.  All scalar types larger
       than 8 bytes are passed by reference.  */
    /* FIXME: delete this */
    if (0 && (AGGREGATE_TYPE_P (type) || int_size_in_bytes (type) > 8))
#else
    if (type != 0
	&& (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	    || TREE_ADDRESSABLE (type)))
#endif
       {
        tree type_ptr_ptr = build_pointer_type (type_ptr);
  
        addr = build1 (INDIRECT_REF, type_ptr,
                      build1 (NOP_EXPR, type_ptr_ptr, valist));

        incr = build2 (PLUS_EXPR, TREE_TYPE (valist),
                      valist, build_int_cst (NULL_TREE, UNITS_PER_WORD));
      }
    else
      {
        HOST_WIDE_INT align, rounded_size;
  
        /* Compute the rounded size of the type.  */
        align = PARM_BOUNDARY / BITS_PER_UNIT;
        rounded_size = (((TREE_INT_CST_LOW (TYPE_SIZE (type)) / BITS_PER_UNIT
                          + align - 1) / align) * align);
  
        /* Align 8 byte operands.  */
        addr = valist;
        if (TYPE_ALIGN (type) > BITS_PER_WORD)
          {
            /* AP = (TYPE *)(((int)AP + 7) & -8)  */
  
            addr = build1 (NOP_EXPR, integer_type_node, valist);
            addr = fold (build2 (PLUS_EXPR, integer_type_node, addr,
                                build_int_cst (NULL_TREE, 7)));
            addr = fold (build2 (BIT_AND_EXPR, integer_type_node, addr,
                                build_int_cst (NULL_TREE, -8)));
            addr = fold (build1 (NOP_EXPR, TREE_TYPE (valist), addr));
          }
  
        /* The increment is always rounded_size past the aligned pointer.  */
        incr = fold (build2 (PLUS_EXPR, TREE_TYPE (addr), addr,
                            build_int_cst (NULL_TREE, rounded_size)));
  
        /* Adjust the pointer in big-endian mode.  */
        if (BYTES_BIG_ENDIAN)
          {
            HOST_WIDE_INT adj;
            adj = TREE_INT_CST_LOW (TYPE_SIZE (type)) / BITS_PER_UNIT;
            if (rounded_size > align)
              adj = rounded_size;
  
            addr = fold (build2 (PLUS_EXPR, TREE_TYPE (addr), addr,
                                build_int_cst (NULL_TREE, rounded_size - adj)));
          }
      }
  
    /* Evaluate the data address.  */
    addr_rtx = expand_expr (addr, NULL_RTX, Pmode, EXPAND_NORMAL);
    addr_rtx = copy_to_reg (addr_rtx);
    
    /* Compute new value for AP.  */
    incr = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist, incr);
    TREE_SIDE_EFFECTS (incr) = 1;
    expand_expr (incr, const0_rtx, VOIDmode, EXPAND_NORMAL);
  
    return addr_rtx;
  }

/* Return a pointer to a function's name with any
   and all prefix encodings stripped from it.  */
const char *
arc_strip_name_encoding (const char *name)
{
  switch (*name)
    {
    case SIMPLE_CALL_FLAG_CHAR:
    case LONG_CALL_FLAG_CHAR:
    case SHORT_CALL_FLAG_CHAR:
      name++;
    }
  return (name) + ((name)[0] == '*') ;
}



/* An address that needs to be expressed as an explicit sum of pcl + offset.  */
int
arc_legitimate_pc_offset_p (rtx addr)
{
  if (GET_CODE (addr) != CONST)
    return 0;
  addr = XEXP (addr, 0);
  if (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 1)) != CONST_INT)
	return 0;
      addr = XEXP (addr, 0);
    }
  return (GET_CODE (addr) == UNSPEC
	  && XVECLEN (addr, 0) == 1
	  && XINT (addr, 1) == ARC_UNSPEC_GOT
	  && GET_CODE (XVECEXP (addr, 0, 0)) == SYMBOL_REF);
}

/* check whether it is a valid pic address or not 
 * A valid pic address on arc should look like
 * const (unspec (SYMBOL_REF/LABEL) (ARC_UNSPEC_GOTOFF/ARC_UNSPEC_GOT))
 */
int
arc_legitimate_pic_addr_p (rtx addr)
{
  if (GET_CODE (addr) != CONST)
    return 0;
    
  addr = XEXP (addr, 0);


  if (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 1)) != CONST_INT)
	return 0;
      addr = XEXP (addr, 0);
    }

  if (GET_CODE (addr) != UNSPEC
      || XVECLEN (addr, 0) != 1)
    return 0;

  /* Must be @GOT or @GOTOFF.  */
  if (XINT (addr, 1) != ARC_UNSPEC_GOT
      && XINT (addr, 1) != ARC_UNSPEC_GOTOFF)
    return 0;

  if (GET_CODE (XVECEXP (addr, 0, 0)) != SYMBOL_REF
      && GET_CODE (XVECEXP (addr, 0, 0)) != LABEL_REF)
    return 0;

  return 1;
}



/* Returns 1 if OP contains a symbol reference */

int
symbolic_reference_mentioned_p (rtx op)
{
  register const char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return 1;
	}

      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return 1;
    }

  return 0;
}

int
arc_raw_symbolic_reference_mentioned_p (rtx op)
{
  register const char *fmt;
  register int i;

  if (GET_CODE(op) == UNSPEC)
    return 0;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
	  return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (arc_raw_symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return 1;
	}

      else if (fmt[i] == 'e' && arc_raw_symbolic_reference_mentioned_p (XEXP (op, i)))
	return 1;
    }

  return 0;
}

/* Legitimize a pic address reference
 *    orig = src
 *    oldx = target if reload_in_progress
 *           src       otherwise
 */
rtx
arc_legitimize_pic_address (rtx orig, rtx oldx)
{
  rtx addr = orig;
  rtx new = orig;
  rtx base;
  
  if (oldx == orig)
    oldx = NULL;

  if (GET_CODE (addr) == LABEL_REF
      || (GET_CODE (addr) == SYMBOL_REF
	  && (CONSTANT_POOL_ADDRESS_P (addr)
	      || SYMBOL_REF_FLAG (addr))))
    {
      /* This symbol may be referenced via a displacement from the PIC
	 base address (@GOTOFF).  */

      current_function_uses_pic_offset_table = 1;
      new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), ARC_UNSPEC_GOTOFF);
      new = gen_rtx_CONST (Pmode, new);
      new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new);

      if (oldx == NULL)
	oldx = gen_reg_rtx (Pmode);

      if (oldx != 0)
	{
	  emit_move_insn (oldx, new);
	  new = oldx;
	}

    }
  else if (GET_CODE (addr) == SYMBOL_REF)
    {
      /* This symbol must be referenced via a load from the
	 Global Offset Table (@GOTPC). */
  
      new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), ARC_UNSPEC_GOT);
      new = gen_rtx_CONST (Pmode, new);
      new = gen_const_mem (Pmode, new);
      
      if (oldx == 0)
	oldx = gen_reg_rtx (Pmode);
      
      emit_move_insn (oldx, new);
      new = oldx;
    }      
  else
    {
      if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);
	  if (GET_CODE (addr) == UNSPEC)
	    {
	      /* Check that the unspec is one of the ones we generate? */
	    }
	  else 
	    gcc_assert (GET_CODE (addr) == PLUS);
	}
      
      if (GET_CODE (addr) == PLUS)
	{
	  rtx op0 = XEXP (addr, 0), op1 = XEXP (addr, 1);

	  /* Check first to see if this is a constant offset from a @GOTOFF
	     symbol reference.  */
	  if ((GET_CODE (op0) == LABEL_REF
	       || (GET_CODE (op0) == SYMBOL_REF
		   && (CONSTANT_POOL_ADDRESS_P (op0)
		       || SYMBOL_REF_FLAG (op0))))
	      && GET_CODE (op1) == CONST_INT)
	    {
	      current_function_uses_pic_offset_table = 1;
	      new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op0), ARC_UNSPEC_GOTOFF);
	      new = gen_rtx_PLUS (Pmode, new, op1);
	      new = gen_rtx_CONST (Pmode, new);
	      new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new);

	      if (oldx != 0)
		{
		  emit_move_insn (oldx, new);
		  new = oldx;
		}
	    }
	  else
	    {
	      base = arc_legitimize_pic_address (XEXP (addr, 0), oldx);
	      new  = arc_legitimize_pic_address (XEXP (addr, 1),
					     base == oldx ? NULL_RTX : oldx);

	      if (GET_CODE (new) == CONST_INT)
		new = plus_constant (base, INTVAL (new));
	      else
		{
		  if (GET_CODE (new) == PLUS && CONSTANT_P (XEXP (new, 1)))
		    {
		      base = gen_rtx_PLUS (Pmode, base, XEXP (new, 0));
		      new = XEXP (new, 1);
		    }
		  new = gen_rtx_PLUS (Pmode, base, new);
		}
	    }
	}
    }

 return new;
}

void 
arc_output_pic_addr_const (FILE * file, rtx x, int code)
{
  char buf[256];

 restart:
  switch (GET_CODE (x))
    {
    case PC:
      if (flag_pic)
	putc ('.', file);
      else
	gcc_unreachable ();
      break;

    case SYMBOL_REF:
      output_addr_const (file, x);

      /* Local functions do not get references through the PLT */
      if (code == 'P' && ! SYMBOL_REF_FLAG (x))
	fputs ("@plt", file);
      break;

    case LABEL_REF:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (XEXP (x, 0)));
      arc_assemble_name (file, buf);
      break;

    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
      arc_assemble_name (file, buf);
      break;

    case CONST_INT:
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      break;

    case CONST:
      arc_output_pic_addr_const (file, XEXP (x, 0), code);
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	{
	  /* We can use %d if the number is one word and positive.  */
	  if (CONST_DOUBLE_HIGH (x))
	    fprintf (file, HOST_WIDE_INT_PRINT_DOUBLE_HEX,
		     CONST_DOUBLE_HIGH (x), CONST_DOUBLE_LOW (x));
	  else if  (CONST_DOUBLE_LOW (x) < 0)
	    fprintf (file, HOST_WIDE_INT_PRINT_HEX, CONST_DOUBLE_LOW (x));
	  else
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_DOUBLE_LOW (x));
	}
      else
	/* We can't handle floating point constants;
	   PRINT_OPERAND must handle them.  */
	output_operand_lossage ("floating constant misused");
      break;

    case PLUS:
      /* FIXME: Not needed here */
      /* Some assemblers need integer constants to appear last (eg masm).  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  arc_output_pic_addr_const (file, XEXP (x, 1), code);
	  fprintf (file, "+");
	  arc_output_pic_addr_const (file, XEXP (x, 0), code);
	}
      else if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  arc_output_pic_addr_const (file, XEXP (x, 0), code);
	  if (INTVAL (XEXP (x, 1)) >= 0)
	    fprintf (file, "+");
	  arc_output_pic_addr_const (file, XEXP (x, 1), code);
	}
      else
	gcc_unreachable();
      break;

    case MINUS:
      /* Avoid outputting things like x-x or x+5-x,
	 since some assemblers can't handle that.  */
      x = simplify_subtraction (x);
      if (GET_CODE (x) != MINUS)
	goto restart;

      arc_output_pic_addr_const (file, XEXP (x, 0), code);
      fprintf (file, "-");
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) < 0)
	{
	  fprintf (file, "(");
	  arc_output_pic_addr_const (file, XEXP (x, 1), code);
	  fprintf (file, ")");
	}
      else
	arc_output_pic_addr_const (file, XEXP (x, 1), code);
      break;

    case ZERO_EXTEND:
    case SIGN_EXTEND:
      arc_output_pic_addr_const (file, XEXP (x, 0), code);
      break;


    case UNSPEC:
      gcc_assert (XVECLEN (x, 0) == 1);
      if (XINT (x, 1) == ARC_UNSPEC_GOT)
	fputs ("pcl,", file);
      arc_output_pic_addr_const (file, XVECEXP (x, 0, 0), code);
      switch (XINT (x, 1))
 	{
 	case ARC_UNSPEC_GOT:
 	  fputs ("@gotpc", file);
 	  break;
 	case ARC_UNSPEC_GOTOFF:
 	  fputs ("@gotoff", file);
 	  break;
 	case ARC_UNSPEC_PLT:
 	  fputs ("@plt", file);
 	  break;
 	default:
	  fprintf(stderr, "%d seen\n",XINT (x,1));
 	  output_operand_lossage ("invalid UNSPEC as operand");

 	  break;
 	}
       break;

    default:
      output_operand_lossage ("invalid expression as operand");
    }
}

/* Emit insns to move operands[1] into operands[0].  */

void
emit_pic_move (rtx *operands, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx temp = reload_in_progress ? operands[0] : gen_reg_rtx (Pmode);

  if (GET_CODE (operands[0]) == MEM && SYMBOLIC_CONST (operands[1]))
    operands[1] = force_reg (Pmode, operands[1]);
  else
    operands[1] = arc_legitimize_pic_address (operands[1], temp);
    
}


/* Prepend the symbol passed as argument to the name */
static void
arc_encode_symbol (tree decl, const char prefix)
{
  const char *str = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  int len = strlen (str);
  char *newstr;

  if(*str == prefix)
    return;
  newstr = (char*) obstack_alloc (&arc_local_obstack, len + 2);

  strcpy (newstr + 1, str);
  *newstr = prefix;
  XSTR (XEXP (DECL_RTL (decl), 0), 0) = newstr;

  return;

}

/* Output to FILE a reference to the assembler name of a C-level name NAME.
   If NAME starts with a *, the rest of NAME is output verbatim.
   Otherwise NAME is transformed in an implementation-defined way
   (usually by the addition of an underscore).
   Many macros in the tm file are defined to call this function.  */
/* FIXME: This can be deleted */
void
arc_assemble_name (FILE *file, const char *name)
{
  const char *real_name=name;

  /*real_name = arc_strip_name_encoding (name);*/
  assemble_name(file, real_name);

}

/* The function returning the number of words, at the beginning of an
   argument, must be put in registers.  The returned value must be
   zero for arguments that are passed entirely in registers or that
   are entirely pushed on the stack.

   On some machines, certain arguments must be passed partially in
   registers and partially in memory.  On these machines, typically
   the first N words of arguments are passed in registers, and the
   rest on the stack.  If a multi-word argument (a `double' or a
   structure) crosses that boundary, its first few words must be
   passed in registers and the rest must be pushed.  This function
   tells the compiler when this occurs, and how many of the words
   should go in registers.

   `FUNCTION_ARG' for these arguments should return the first register
   to be used by the caller for this argument; likewise
   `FUNCTION_INCOMING_ARG', for the called function.

   The function is used to implement macro FUNCTION_ARG_PARTIAL_NREGS. */

/* if REGNO is the least arg reg available then what is the total number of arg
   regs available */
#define GPR_REST_ARG_REGS(REGNO) ( ((REGNO) <= (MAX_ARC_PARM_REGS))  \
				   ? ((MAX_ARC_PARM_REGS) - (REGNO)) \
                                   : 0 )

/* since arc parm regs are contiguous */
#define ARC_NEXT_ARG_REG(REGNO) ( (REGNO) + 1 )

/* Implement TARGET_ARG_PARTIAL_BYTES.  */

static int
/* arc_function_arg_partial_nregs (cum, mode, type, named) */
arc_arg_partial_bytes (CUMULATIVE_ARGS *cum, enum machine_mode mode, tree type, bool named ATTRIBUTE_UNUSED)
{
  int bytes = (mode == BLKmode
	       ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode));
  int words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  int arg_num = *cum;
  int ret;

  arg_num = ROUND_ADVANCE_CUM (arg_num, mode, type);
  ret = GPR_REST_ARG_REGS (arg_num);

  /* ICEd at function.c:2361, and ret is copied to data->partial */
    ret = (ret >= words ? 0 : ret * UNITS_PER_WORD);

  return ret;
}



/* This function is used to control a function argument is passed in a
   register, and which register.
   
   The arguments are CUM, of type CUMULATIVE_ARGS, which summarizes
   (in a way defined by INIT_CUMULATIVE_ARGS and FUNCTION_ARG_ADVANCE)
   all of the previous arguments so far passed in registers; MODE, the
   machine mode of the argument; TYPE, the data type of the argument
   as a tree node or 0 if that is not known (which happens for C
   support library functions); and NAMED, which is 1 for an ordinary
   argument and 0 for nameless arguments that correspond to `...' in
   the called function's prototype.

   The returned value should either be a `reg' RTX for the hard
   register in which to pass the argument, or zero to pass the
   argument on the stack.

   For machines like the Vax and 68000, where normally all arguments
   are pushed, zero suffices as a definition.

   The usual way to make the ANSI library `stdarg.h' work on a machine
   where some arguments are usually passed in registers, is to cause
   nameless arguments to be passed on the stack instead.  This is done
   by making the function return 0 whenever NAMED is 0.

   You may use the macro `MUST_PASS_IN_STACK (MODE, TYPE)' in the
   definition of this function to determine if this argument is of a
   type that must be passed in the stack.  If `REG_PARM_STACK_SPACE'
   is not defined and the function returns non-zero for such an
   argument, the compiler will abort.  If `REG_PARM_STACK_SPACE' is
   defined, the argument will be computed in the stack and then loaded
   into a register.

   The function is used to implement macro FUNCTION_ARG. */

rtx
arc_function_arg (CUMULATIVE_ARGS *cum, enum machine_mode mode, 
		  tree type ATTRIBUTE_UNUSED, int named ATTRIBUTE_UNUSED)
{
  int arg_num = *cum;
  rtx ret;
  const char *debstr;

  arg_num = ROUND_ADVANCE_CUM (arg_num, mode, type);
  /* Return a marker for use in the call instruction.  */
  if (mode == VOIDmode)
    {
      ret = const0_rtx;
      debstr = "<0>";
    }
  else if (GPR_REST_ARG_REGS (arg_num) > 0)
  {
      ret = gen_rtx_REG (mode, arg_num);
      debstr = reg_names [arg_num];
      
    }
  else
    {
      ret = NULL_RTX;
      debstr = "memory";
    }

  return ret;

}

/* The function to update the summarizer variable *CUM to advance past
   an argument in the argument list.  The values MODE, TYPE and NAMED
   describe that argument.  Once this is done, the variable *CUM is
   suitable for analyzing the *following* argument with
   `FUNCTION_ARG', etc.

   This function need not do anything if the argument in question was
   passed on the stack.  The compiler knows how to track the amount of
   stack space used for arguments without any special help.

   The function is used to implement macro FUNCTION_ARG_ADVANCE. */
/* For the ARC: the cum set here is passed on to function_arg where we
   look at its value and say which reg to use. Strategy: advance the
   regnumber here till we run out of arg regs, then set *cum to last
   reg. In function_arg, since *cum > last arg reg we would return 0
   and thus the arg will end up on the stack. For straddling args of
   course function_arg_partial_nregs will come into play */
void
arc_function_arg_advance (CUMULATIVE_ARGS *cum, enum machine_mode mode, tree type, int named ATTRIBUTE_UNUSED)
{
  int bytes = (mode == BLKmode
	       ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode));
  int words = (bytes + UNITS_PER_WORD  - 1) / UNITS_PER_WORD;
  int i;

  if (words)
    *cum = ROUND_ADVANCE_CUM (*cum, mode, type);
  for (i = 0; i < words; i++)
    *cum = ARC_NEXT_ARG_REG (*cum);

}

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FN_DECL_OR_TYPE is its
   FUNCTION_DECL; otherwise, FN_DECL_OR_TYPE is its type.  */
rtx
arc_function_value (tree valtype, tree fn_decl_or_type ATTRIBUTE_UNUSED,
		    bool outgoing ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = TYPE_MODE (valtype);
  int unsignedp ATTRIBUTE_UNUSED;

  unsignedp = TYPE_UNSIGNED (valtype);
  if (INTEGRAL_TYPE_P (valtype) || TREE_CODE (valtype) == OFFSET_TYPE)
    PROMOTE_MODE(mode, unsignedp, valtype);
  return gen_rtx_REG (mode, 0);
}

/* Returns the return address that is used by builtin_return_address */
rtx
arc_return_addr_rtx (int count, ATTRIBUTE_UNUSED rtx frame)
{
  if (count != 0)
      return const0_rtx;

  if(TARGET_A4)
  {
      /* Only the lower 24 bits of blink are valid */
      rtx temp = get_hard_reg_initial_val (Pmode, RETURN_ADDR_REGNUM);
      emit_insn (gen_andsi3(temp,temp,gen_rtx_CONST_INT (SImode,0x00ffffff)));
      return temp;
  }

  return get_hard_reg_initial_val (Pmode , RETURN_ADDR_REGNUM);
}

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and
   that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */
/* TODO: This should not be a separate function */
bool
arc_legitimate_pic_operand_p (rtx x)
{
  return !arc_raw_symbolic_reference_mentioned_p (x);
}

/* Determine if a given RTX is a valid constant.  We already know this
   satisfies CONSTANT_P.  */
bool
arc_legitimate_constant_p (rtx x)
{
  if (!flag_pic)
    return true;

  switch (GET_CODE (x))
    {
    case CONST:
      x = XEXP (x, 0);

      if (GET_CODE (x) == PLUS)
	{
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    return false;
	  x = XEXP (x, 0);
	}

      /* Only some unspecs are valid as "constants".  */
      if (GET_CODE (x) == UNSPEC)
	switch (XINT (x, 1))
	  {
	  case ARC_UNSPEC_PLT:
	  case ARC_UNSPEC_GOTOFF:
	  case ARC_UNSPEC_GOT:
	  case UNSPEC_PROF:
	    return true;

	  default:
	    gcc_unreachable ();
	  }

      /* We must have drilled down to a symbol.  */
      if ( arc_raw_symbolic_reference_mentioned_p (x))
	return false;

      /* return true */
      break;

    case LABEL_REF:
    case SYMBOL_REF:
      return false;

    default:
      break;
    }

  /* Otherwise we handle everything else in the move patterns.  */
  return true;
}

/* Determine if it's legal to put X into the constant pool. */
static bool
arc_cannot_force_const_mem (rtx x)
{
  return !arc_legitimate_constant_p (x);
}


/* Generic function to define a builtin */
#define def_mbuiltin(MASK, NAME, TYPE, CODE)				\
  do									\
    {									\
       if (MASK)                                                        \
          lang_hooks.builtin_function ((NAME), (TYPE), (CODE), BUILT_IN_MD, NULL, NULL_TREE); \
    }									\
  while (0)


static void
arc_init_builtins (void)
{
    tree endlink = void_list_node;
    
    tree void_ftype_void
	= build_function_type (void_type_node,
			       endlink);
    
    tree int_ftype_int
	= build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, integer_type_node, endlink));

    tree int_ftype_short_int
	= build_function_type (integer_type_node,
			       tree_cons (NULL_TREE, short_integer_type_node, endlink));

    tree void_ftype_int_int
	= build_function_type (void_type_node,
			       tree_cons (NULL_TREE, integer_type_node,
					  tree_cons (NULL_TREE, integer_type_node, endlink)));
    tree void_ftype_usint_usint
	= build_function_type (void_type_node,
			       tree_cons (NULL_TREE, long_unsigned_type_node,
					  tree_cons (NULL_TREE, long_unsigned_type_node, endlink)));

    tree int_ftype_int_int
	= build_function_type (integer_type_node,
			       tree_cons (NULL_TREE, integer_type_node,
					  tree_cons (NULL_TREE, integer_type_node, endlink)));

    tree usint_ftype_usint
	= build_function_type (long_unsigned_type_node,
			   tree_cons (NULL_TREE, long_unsigned_type_node, endlink));

    tree void_ftype_usint
	= build_function_type (void_type_node,
			   tree_cons (NULL_TREE, long_unsigned_type_node, endlink));

    /* Add the builtins */
    def_mbuiltin (1,"__builtin_arc_nop", void_ftype_void, ARC_BUILTIN_NOP);
    def_mbuiltin (TARGET_NORM, "__builtin_arc_norm", int_ftype_int, ARC_BUILTIN_NORM);
    def_mbuiltin (TARGET_NORM, "__builtin_arc_normw", int_ftype_short_int, ARC_BUILTIN_NORMW);
    def_mbuiltin (TARGET_SWAP, "__builtin_arc_swap", int_ftype_int, ARC_BUILTIN_SWAP);
    def_mbuiltin (TARGET_MUL64_SET,"__builtin_arc_mul64", void_ftype_int_int, ARC_BUILTIN_MUL64);
    def_mbuiltin (TARGET_MUL64_SET,"__builtin_arc_mulu64", void_ftype_usint_usint, ARC_BUILTIN_MULU64);
    def_mbuiltin (1,"__builtin_arc_rtie", void_ftype_void, ARC_BUILTIN_RTIE);
    def_mbuiltin (TARGET_ARC700,"__builtin_arc_sync", void_ftype_void, ARC_BUILTIN_SYNC);
    def_mbuiltin ((TARGET_EA_SET && TARGET_ARCOMPACT),"__builtin_arc_divaw", int_ftype_int_int, ARC_BUILTIN_DIVAW);
    def_mbuiltin (1,"__builtin_arc_brk", void_ftype_void, ARC_BUILTIN_BRK);
    def_mbuiltin (1,"__builtin_arc_flag", void_ftype_usint, ARC_BUILTIN_FLAG);
    def_mbuiltin (1,"__builtin_arc_sleep", void_ftype_usint, ARC_BUILTIN_SLEEP);
    def_mbuiltin (1,"__builtin_arc_swi", void_ftype_void, ARC_BUILTIN_SWI);
    def_mbuiltin (1,"__builtin_arc_core_read", usint_ftype_usint, ARC_BUILTIN_CORE_READ);
    def_mbuiltin (1,"__builtin_arc_core_write", void_ftype_usint_usint, ARC_BUILTIN_CORE_WRITE); 
    def_mbuiltin (1,"__builtin_arc_lr", usint_ftype_usint, ARC_BUILTIN_LR);
    def_mbuiltin (1,"__builtin_arc_sr", void_ftype_usint_usint, ARC_BUILTIN_SR); 
    def_mbuiltin (TARGET_ARC700,"__builtin_arc_trap_s", void_ftype_usint, ARC_BUILTIN_TRAP_S);
    def_mbuiltin (TARGET_ARC700,"__builtin_arc_unimp_s", void_ftype_void, ARC_BUILTIN_UNIMP_S);

    if (TARGET_SIMD_SET)    
      arc_init_simd_builtins ();
}

static rtx arc_expand_simd_builtin (tree, rtx, rtx, enum machine_mode, int);

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
arc_expand_builtin (tree exp,
		    rtx target,
		    rtx subtarget ATTRIBUTE_UNUSED,
		    enum machine_mode mode ATTRIBUTE_UNUSED,
		    int ignore ATTRIBUTE_UNUSED)
{
  tree              fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree              arglist = TREE_OPERAND (exp, 1);
  tree              arg0;
  tree              arg1;
  rtx               op0;
  rtx               op1;
  int               fcode = DECL_FUNCTION_CODE (fndecl);
  int               icode;
  enum machine_mode mode0;
  enum machine_mode mode1;

  if (fcode > ARC_SIMD_BUILTIN_BEGIN && fcode < ARC_SIMD_BUILTIN_END)
    return arc_expand_simd_builtin (exp, target, subtarget, mode, ignore);

  switch (fcode)
    {
    case ARC_BUILTIN_NOP:
      emit_insn (gen_nop ());
      return NULL_RTX;

    case ARC_BUILTIN_NORM:
      icode = CODE_FOR_norm;
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      mode0 =  insn_data[icode].operand[1].mode;
      target = gen_reg_rtx (SImode);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      emit_insn (gen_norm (target,op0));
      return target;

    case ARC_BUILTIN_NORMW:

	/* FIXME : This should all be HI mode, not SI mode */
	icode = CODE_FOR_normw;
	arg0 = TREE_VALUE (arglist);
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	mode0 =  insn_data[icode].operand[1].mode;
	target = gen_reg_rtx (SImode);
	
	if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	  op0 = copy_to_mode_reg (mode0, convert_to_mode (mode0, op0,0));
	
	emit_insn (gen_normw (target, op0));
	return target;
	
    case ARC_BUILTIN_MUL64:
	icode = CODE_FOR_mul64;
	arg0 = TREE_VALUE (arglist);
	arg1 = TREE_VALUE (TREE_CHAIN (arglist));
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
	
	mode0 =  insn_data[icode].operand[0].mode;
	mode1 =  insn_data[icode].operand[1].mode;
	
	if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
	
	if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

	emit_insn (gen_mul64 (op0,op1));
	return NULL_RTX;

    case ARC_BUILTIN_MULU64:
	icode = CODE_FOR_mulu64;
	arg0 = TREE_VALUE (arglist);
	arg1 = TREE_VALUE (TREE_CHAIN (arglist));
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
	
	mode0 =  insn_data[icode].operand[0].mode;
	mode1 =  insn_data[icode].operand[1].mode;
	
	if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
	
	if (! (*insn_data[icode].operand[0].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

	emit_insn (gen_mulu64 (op0,op1));
	return NULL_RTX;

    case ARC_BUILTIN_RTIE:
	icode = CODE_FOR_rtie;
	emit_insn (gen_rtie (const1_rtx));
	return NULL_RTX;

    case ARC_BUILTIN_SYNC:
	icode = CODE_FOR_sync;
	emit_insn (gen_sync (const1_rtx));
	return NULL_RTX;

    case ARC_BUILTIN_SWAP:
	icode = CODE_FOR_swap;
	arg0 = TREE_VALUE (arglist);
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	mode0 =  insn_data[icode].operand[1].mode;
	target = gen_reg_rtx (SImode);

	if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
	
	emit_insn (gen_swap (target,op0));
	return target;

    case ARC_BUILTIN_DIVAW:
	icode = CODE_FOR_divaw;
	arg0 = TREE_VALUE (arglist);
	arg1 = TREE_VALUE (TREE_CHAIN (arglist));
	
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
	target = gen_reg_rtx (SImode);

	mode0 =  insn_data[icode].operand[0].mode;
	mode1 =  insn_data[icode].operand[1].mode;
	
	if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	    op0 = copy_to_mode_reg (mode0, op0);
	
	if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
	    op1 = copy_to_mode_reg (mode1, op1);
	
	emit_insn (gen_divaw (target,op0,op1));
	return target;

    case ARC_BUILTIN_BRK:
	icode = CODE_FOR_brk;
	emit_insn (gen_brk (const1_rtx));
	return NULL_RTX;

    case ARC_BUILTIN_SLEEP:
	icode = CODE_FOR_sleep;
	arg0 = TREE_VALUE (arglist);

	fold (arg0);
	
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	mode0 = insn_data[icode].operand[1].mode;

	emit_insn (gen_sleep (op0));
	return NULL_RTX;

    case ARC_BUILTIN_SWI:
	icode = CODE_FOR_swi;
	emit_insn (gen_swi (const1_rtx));
	return NULL_RTX;
	
    case ARC_BUILTIN_FLAG:
	icode = CODE_FOR_flag;
	arg0 = TREE_VALUE (arglist);
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	mode0 =  insn_data[icode].operand[0].mode;

	if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	  op0 = copy_to_mode_reg (mode0, op0);
	
	emit_insn (gen_flag (op0));
	return NULL_RTX;
      
    case ARC_BUILTIN_CORE_READ:
	icode = CODE_FOR_core_read;
	arg0 = TREE_VALUE (arglist);
	target = gen_reg_rtx (SImode);

	fold (arg0);
	
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	mode0 = insn_data[icode].operand[1].mode;
	
	emit_insn (gen_core_read (target, op0));
	return target;

    case ARC_BUILTIN_CORE_WRITE:
	icode = CODE_FOR_core_write;
	arg0 = TREE_VALUE (arglist);
	arg1 = TREE_VALUE (TREE_CHAIN (arglist));
	
	fold (arg1);
	
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);

	mode0 = insn_data[icode].operand[0].mode;
	mode1 = insn_data[icode].operand[1].mode;

	emit_insn (gen_core_write (op0, op1));
	return NULL_RTX;

    case ARC_BUILTIN_LR:
	icode = CODE_FOR_lr;
	arg0 = TREE_VALUE (arglist);
	target = gen_reg_rtx (SImode);

	fold (arg0);
	
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	mode0 = insn_data[icode].operand[1].mode;
	
	emit_insn (gen_lr (target, op0));
	return target;

    case ARC_BUILTIN_SR:
	icode = CODE_FOR_sr;
	arg0 = TREE_VALUE (arglist);
	arg1 = TREE_VALUE (TREE_CHAIN (arglist));
	
	fold (arg1);
	
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);

	mode0 = insn_data[icode].operand[0].mode;
	mode1 = insn_data[icode].operand[1].mode;

	emit_insn (gen_sr (op0, op1));
	return NULL_RTX;

    case ARC_BUILTIN_TRAP_S:
	icode = CODE_FOR_trap_s;
	arg0 = TREE_VALUE (arglist);

	fold (arg0);
	
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	mode0 = insn_data[icode].operand[1].mode;

	emit_insn (gen_trap_s (op0));
	return NULL_RTX;

    case ARC_BUILTIN_UNIMP_S:
	icode = CODE_FOR_unimp_s;
	emit_insn (gen_unimp_s (const1_rtx));
	return NULL_RTX;

    default:
	break;
    }

  /* @@@ Should really do something sensible here.  */
  return NULL_RTX;
}

/* Returns if the operands[ opno ] is a valid compile-time constant to be used
   as register number in the code for builtins. Else it flags an error. */

int
check_if_valid_regno_const (rtx *operands, int opno)
{
    
  switch (GET_CODE (operands[opno]))
    {
    case SYMBOL_REF :
    case CONST :
    case CONST_INT :
      return 1;
    default:
	error("register number must be a compile-time constant. Try giving higher optimization levels");
	break;
    }
  return 0;
}

/* Check that after all the constant folding, whether the operand to
   __builtin_arc_sleep is an unsigned int of 6 bits. If not, flag an error
*/
int
check_if_valid_sleep_operand (rtx *operands, int opno)
{
  switch (GET_CODE (operands[opno]))
    {
    case CONST :
    case CONST_INT :
	if( UNSIGNED_INT6 (INTVAL (operands[opno])))
	    return 1;
    default:
	fatal_error("operand for sleep instruction must be a unsigned 6 bit compile-time constant.");
	break;
    }
  return 0;
}

/* Return nonzero if it is ok to make a tail-call to DECL.  */
static bool
arc_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  const char * fname;

  if (!TARGET_ARCOMPACT)
    {
      /* Never tailcall something for which we have no decl.  */
      if (decl == NULL)
	return false;
    
      /* Extract the function name from the decl node */
      fname = XSTR (XEXP (DECL_RTL (decl), 0), 0);
    
      /* ARC does not have a branch [reg], so no sibcalls with -mlong-calls, unless
	 the called function has short_call attribute set */
      if (TARGET_LONG_CALLS_SET && !ARC_ENCODED_SHORT_CALL_ATTR_P(fname))
	return false;
    
      /* Is this a long_call attributed function. If so, return false */
      if (ARC_ENCODED_LONG_CALL_ATTR_P(fname))
	return false;
    }

  /* Never tailcall from an ISR routine - it needs a special exit sequence.  */
  if (ARC_INTERRUPT_P (arc_compute_function_type (current_function_decl)))
    return false;

  /* Everything else is ok.  */
  return true;
}

/* Output the code to jump based on the switch table */
const char *
arc_output_casesi_insn (rtx  *operands)
{
  if(flag_pic)
	output_asm_insn("add %2,r26,%^%1@gotoff",operands);
  else
	output_asm_insn ("mov %2,%^%1", operands);

  if (TARGET_A4)
  {
    if (TARGET_BARREL_SHIFTER_SET)
      output_asm_insn ("asl %3,%0,2", operands);
    else
      output_asm_insn ("asl %3,%0\n\tasl %3,%3", operands);

    output_asm_insn ("ld %2,[%2,%3]", operands);
  }
  else
  {
    if(flag_pic)
    {
      output_asm_insn ("ld.as %3,[%2,%0]", operands);
      output_asm_insn ("add %2,%2,%3",operands);
    }
    else
  	output_asm_insn ("ld.as %2,[%2,%0]", operands);
  }

  if (TARGET_ARCOMPACT && COMPACT_GP_REG_P(REGNO(operands[2])))
    output_asm_insn ("j_s%* [%a2] ;;the switch case  ", operands);
  else
    output_asm_insn ("j%* [%a2] ;;the switch case  ", operands);

  return "";
}

/* Output code to add DELTA to the first argument, and then jump
   to FUNCTION.  Used for C++ multiple inheritance.  */
static void
arc_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
		     HOST_WIDE_INT delta,
		     HOST_WIDE_INT vcall_offset,
		     tree function)
{
    int mi_delta = delta;
    const char *const mi_op = mi_delta < 0 ? "sub" : "add";
    int shift = 0;
    int this_regno = (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function)
		      ? 1 : 0);
    const char *fname;
    if (mi_delta < 0) 
	mi_delta = - mi_delta; 
    
    /* Add DELTA.  When possible use a plain add, otherwise load it into
       a register first. */
    
    while (mi_delta != 0)
    {
	if ((mi_delta & (3 << shift)) == 0)
	    shift += 2;
	else
	{
	    asm_fprintf (file, "\t%s\t%s, %s, %d\n",
			 mi_op, reg_names[this_regno], reg_names[this_regno],
			 mi_delta & (0xff << shift));
	    mi_delta &= ~(0xff << shift);
	    shift += 8;
	}
    }
    
   /* If needed, add *(*THIS + VCALL_OFFSET) to THIS.  */
    if (vcall_offset != 0)
    {
	
   /*      ld  r12,[this]           --> temp = *this
	   add r12,r12,vcall_offset --> temp = *(*this + vcall_offset)
	   ld r12,[r12]           
	   add this,this,r12        --> this+ = *(*this + vcall_offset)
   */
	asm_fprintf (file, "\tld\t%s, [%s]\n",
		     ARC_TEMP_SCRATCH_REG, reg_names[this_regno]);
	asm_fprintf (file, "\tadd\t%s, %s, %ld\n",
		     ARC_TEMP_SCRATCH_REG, ARC_TEMP_SCRATCH_REG, vcall_offset);
	asm_fprintf (file, "\tld\t%s, [%s]\n",
		     ARC_TEMP_SCRATCH_REG, ARC_TEMP_SCRATCH_REG);
	asm_fprintf (file, "\tadd\t%s, %s, %s\n",
		     reg_names[this_regno], reg_names[this_regno], ARC_TEMP_SCRATCH_REG);
    }
    
  fname = XSTR (XEXP (DECL_RTL (function), 0), 0);
  if (TARGET_LONG_CALLS_SET
      ? !ARC_ENCODED_SHORT_CALL_ATTR_P (fname)
      : ARC_ENCODED_LONG_CALL_ATTR_P (fname))
    fputs ("\tj\t", file);
  else
    fputs ("\tb\t", file);
  assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
  fputc ('\n', file);
}

/* Return nonzero if a 32 bit "long_call" should be generated for
   this call.  We generate a long_call if the function:

        a.  has an __attribute__((long call))
     or b.  the -mlong-calls command line switch has been specified

   However we do not generate a long call if the function has an
   __attribute__ ((short_call))
   
   This function will be called by C fragments contained in the machine
   description file.  CALL_SYMBOL is used to distinguish between
   two different callers of the function.  It is set to 1 in the
   "call_symbol" and "call_symbol_value" patterns and to 0 in the "call"
   and "call_value" patterns.  This is because of the difference in the
   SYM_REFs passed by these patterns.  */
int
arc_is_longcall_p (rtx sym_ref, int call_symbol)
{

  if (!call_symbol)
    {
      if (GET_CODE (sym_ref) != MEM)
	return 0;

      sym_ref = XEXP (sym_ref, 0);
    }

  if (GET_CODE (sym_ref) != SYMBOL_REF)
    return 0;
  
  return  ARC_ENCODED_LONG_CALL_ATTR_P (XSTR (sym_ref, 0)) 
    || ( TARGET_LONG_CALLS_SET && !ARC_ENCODED_SHORT_CALL_ATTR_P (XSTR (sym_ref,0)));

}

/* Emit profiling code for calling CALLEE.  Return nonzero if a special
   call pattern needs to be generated.  */
int
arc_profile_call (rtx callee)
{
  rtx from = XEXP (DECL_RTL (current_function_decl), 0);

  if (CONSTANT_P (callee))
    {
      rtx count_ptr
	= gen_rtx_CONST (Pmode,
			 gen_rtx_UNSPEC (Pmode,
					 gen_rtvec (3, from, callee,
						    CONST0_RTX (Pmode)),
					 UNSPEC_PROF));
      rtx counter = gen_rtx_MEM (SImode, count_ptr);
      /* ??? The increment would better be done atomically, but as there is
	 no proper hardware support, that would be too expensive.  */
      emit_move_insn (counter, force_reg (SImode, plus_constant (counter, 1)));
      return 0;
    }
  else
    {
      rtx count_list_ptr
	= gen_rtx_CONST (Pmode,
			 gen_rtx_UNSPEC (Pmode,
					 gen_rtvec (3, from, CONST0_RTX (Pmode),
						    CONST0_RTX (Pmode)),
					 UNSPEC_PROF));
      emit_move_insn (gen_rtx_REG (Pmode, 8), count_list_ptr);
      emit_move_insn (gen_rtx_REG (Pmode, 9), callee);
      return 1;
    }
}

/* ashwin : taken from gcc-4.2-FSF clean sources */
/* For ARC, All aggregates and arguments greater than 8 bytes are
   passed by reference.  */
static bool
arc_pass_by_reference (CUMULATIVE_ARGS *ca ATTRIBUTE_UNUSED,
		       enum machine_mode mode ATTRIBUTE_UNUSED, 
		       tree type ATTRIBUTE_UNUSED,
		       bool named ATTRIBUTE_UNUSED)
{
  return (type != 0
	  && (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	      || TREE_ADDRESSABLE (type)));

/*   ashwin : We always pass arguments are passed by value  */
  return 0;

  /*   unsigned HOST_WIDE_INT size; */

/*   if (type) */
/*     { */
/*       if (AGGREGATE_TYPE_P (type)) */
/* 	return true; */
/*       size = int_size_in_bytes (type); */
/*     } */
/*   else */
/*     size = GET_MODE_SIZE (mode); */

/*   return size > 8; */
}
/* ~ashwin */


/* NULL if INSN insn is valid within a low-overhead loop.
   Otherwise return why doloop cannot be applied.  */

static const char *
arc_invalid_within_doloop (rtx insn)
{
  if (CALL_P (insn))
    return "Function call in the loop.";
  return NULL;
}

/* ARC's machince specific reorg function.  */
static void
arc_reorg (void)
{
  rtx insn, pattern;
  rtx pc_target;
  long offset;
  int changed;
 
  /* Emit special sections for profiling.  */
  if (current_function_profile)
    {
      section *save_text_section;
      rtx insn;
      int size = get_max_uid () >> 4;
      htab_t htab = htab_create (size, unspec_prof_hash, unspec_prof_htab_eq,
				 NULL);

      save_text_section = in_section;
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if (NONJUMP_INSN_P (insn))
	  note_stores (PATTERN (insn), write_profile_sections, htab);
      if (htab_elements (htab))
	in_section = 0;
      switch_to_section (save_text_section);
      htab_delete (htab);
    }

  /* Link up loop ends with their loop start.  */
  {
    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == JUMP_INSN
	  && recog_memoized (insn) == CODE_FOR_doloop_end_i)
	{
	  rtx num = GEN_INT (CODE_LABEL_NUMBER (JUMP_LABEL (insn)));
	  rtx lp, prev = prev_nonnote_insn (JUMP_LABEL (insn));
	  rtx op0 = XEXP (XVECEXP (PATTERN (insn), 0, 1), 0);

	  for (lp = prev;
	       (lp && NONJUMP_INSN_P (lp)
		&& recog_memoized (lp) != CODE_FOR_doloop_begin_i);
	       lp = prev_nonnote_insn (lp))
	    ;
	  if (prev && NONJUMP_INSN_P (lp)
	      && !dead_or_set_regno_p (lp, LP_COUNT))
	    {
	      rtx begin_cnt = XEXP (XVECEXP (PATTERN (lp), 0 ,3), 0);

	      if (INTVAL (XEXP (XVECEXP (PATTERN (lp), 0, 4), 0)))
		/* The loop end insn has been duplicated.  That can happen
		   when there is a conditional block at the very end of
		   the loop.  */
		goto failure;
	      if (true_regnum (op0) != LP_COUNT || !REG_P (begin_cnt))
		{
		  /* Register allocation failed to allocate to the right
		     register.  There is no point into teaching reload to
		     fix this up with reloads, as that would cost more
		     than using an ordinary core register with the
		     doloop_fallback pattern.  */
		  remove_insn (lp);
		  goto failure;
		}
	      /* It is common that the optimizers copy the loop count from
		 another register, and doloop_begin_i is stuck with the
		 source of the move.  Making doloop_begin_i only accept "l"
		 is nonsentical, as this then makes reload evict the pseudo
		 used for the loop end.  The underlying cause is that the
		 optimizers don't understand that the register allocation for
		 doloop_begin_i should be treated as part of the loop.
		 Try to work around this problem by verifying the previous
		 move exists.  */
	      if (true_regnum (begin_cnt) != LP_COUNT)
		{
		  rtx mov, set, note;

		  for (mov = prev_nonnote_insn (lp); mov;
		       mov = prev_nonnote_insn (mov))
		    {
		      if (!NONJUMP_INSN_P (mov))
			mov = 0;
		      else if ((set = single_set (mov))
			  && rtx_equal_p (SET_SRC (set), begin_cnt)
			  && rtx_equal_p (SET_DEST (set), op0))
			break;
		    }
		  if (mov)
		    {
		      XEXP (XVECEXP (PATTERN (lp), 0 ,3), 0) = op0;
		      note = find_regno_note (lp, REG_DEAD, REGNO (begin_cnt));
		      if (note)
			remove_note (lp, note);
		    }
		  else
		    {
		      remove_insn (lp);
		      goto failure;
		    }
		}
	      XEXP (XVECEXP (PATTERN (insn), 0, 4), 0) = num;
	      XEXP (XVECEXP (PATTERN (lp), 0, 4), 0) = num;
	      if (prev != lp)
		{
		  remove_insn (lp);
		  add_insn_after (lp, prev);
		}
	    }
	  else
	    {
	      /* Sometimes the loop optimizer makes a complete hash of the
		 loop.  If it were only that the loop is not entered at the
		 top, we could fix this up by setting LP_START with SR .
		 However, if we can't find the loop begin were it should be,
		 chances are that it does not even dominate the loop, but is
		 inside the loop instead.  Using SR there would kill
		 performance.
		 We use the doloop_fallback pattern here, which executes
		 in two cycles on the ARC700 when predicted correctly.  */
	    failure:
	      if (!REG_P (op0))
		{
		  rtx op3 = XEXP (XVECEXP (PATTERN (insn), 0, 5), 0);

		  emit_insn_before (gen_move_insn (op3, op0), insn);
		  PATTERN (insn)
		    = gen_doloop_fallback_m (op3, JUMP_LABEL (insn), op0);
		}
	      else
		XVEC (PATTERN (insn), 0)
		  = gen_rtvec (2, XVECEXP (PATTERN (insn), 0, 0),
			       XVECEXP (PATTERN (insn), 0, 1));
	      INSN_CODE (insn) = -1;
	    }
	}
    }

  /* Generate BRcc insns, by combining cmp and Bcc insns wherever possible */
   /* BRcc only for arcompact ISA */
   if (!TARGET_ARCOMPACT || TARGET_NO_BRCC_SET)
     return;
 
/*    /\* Compute LOG_LINKS.  *\/ */
/*    for (bb = 0; bb < current_nr_blocks; bb++) */
/*      compute_block_backward_dependences (bb); */

   do
     {
       init_insn_lengths();
       changed = 0;
 
       /* Call shorten_branches to calculate the insn lengths */
       shorten_branches (get_insns());
       
       for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
 	{
 	  rtx log_link, label;
 	  enum attr_type insn_type;
 	  
 	  /* If a non-jump insn (or a casesi jump table), continue */
 	  if (GET_CODE (insn) != JUMP_INSN ||
 	      GET_CODE (PATTERN (insn)) == ADDR_VEC
 	      || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
 	    continue;
 
 	  if ((insn_type =  get_attr_type (insn)) == TYPE_BRCC
 	      || insn_type == TYPE_BRCC_NO_DELAY_SLOT)
 	    continue;
 	  
 	  /* OK. so we have a jump insn */
 	  /* We need to check that it is a bcc */
 	  /* Bcc => set (pc) (if_then_else ) */
 	  pattern = PATTERN (insn);
 	  if (GET_CODE (pattern) != SET ||
 	      GET_CODE (SET_SRC(pattern)) != IF_THEN_ELSE)
 	    continue;
 	  
 	  
 	  if (!INSN_ADDRESSES_SET_P())
 	    fatal_error ("Insn addresses not set before reorg");
 	  
 	  /* Now check if the jump is beyond the s9 range */
 	  offset = branch_dest(insn)-INSN_ADDRESSES(INSN_UID(insn));
 	  
 	  if(offset > 253 || offset < -254)
 	    continue;
 	  
 	  pc_target = SET_SRC (pattern);
 	  
 	  
 	  /* Now go back and search for the set cc insn */
 	  log_link = LOG_LINKS (insn);
 	  
 	  label = XEXP (pc_target, 1);

#if 0 	  
 	  /* Traverse all the back links for this insn */
 	  for (log_link = LOG_LINKS (insn); log_link; log_link = XEXP (log_link, 1))
#endif
 	    {
	      rtx pat, scan, link_insn = NULL;
	      int set = 0;
	      
	      for (scan = PREV_INSN (insn);
		   scan && GET_CODE (scan) != CODE_LABEL;
		   scan = PREV_INSN (scan))
		{
		  if (! INSN_P (scan))
		    continue;

		  pat = PATTERN (scan);

		  if ((GET_CODE (pat) == SET 
			&& cc_register (SET_DEST (pat), VOIDmode)))
		    set = 1;
		    
		  if (set)
		    {
		      link_insn = scan;
		      break;
		    }
		}
	      
	  
	      if (! link_insn)
		continue;

	      pat = PATTERN (link_insn);

 	      /* Check if this is a data dependency */
 	      if (GET_CODE (pat) == SET 
 		  && cc_register (SET_DEST (pat), VOIDmode))
 		{
 		  rtx operator, brcc_s_possible_rtx, op1, op2, brcc_insn;
 		  
 		  /* ok this is the set cc. copy args here */
 		  operator = XEXP (pc_target, 0);
 		  
		  if (!register_operand (XEXP ( SET_SRC (pat), 0), VOIDmode))
		    continue;

 		  /* None of the two cmp operands should be set between the 
 		     cmp and the branch */
 		  if (reg_set_between_p (XEXP ( SET_SRC (pat), 0), link_insn, insn))
 		    continue;
 		  
 		  if (reg_set_between_p (XEXP ( SET_SRC (pat), 1), link_insn, insn))
 		    continue;
 		  
 		  /* Since the MODE check does not work, check that this is 
 		     CC reg's last set location before insn, and also no instruction
		     between the cmp and branch uses the condition codes */
 		  if ((reg_set_between_p (SET_DEST (pat), link_insn, insn))
		      || (reg_used_between_p (SET_DEST (pat), link_insn, insn)))
 		    continue;
 		  
 		  /* CC reg shd be dead after insn */
 		  if (!find_regno_note (insn, REG_DEAD, 61))
 		    continue;
		  if (!register_operand (XEXP ( SET_SRC (pat), 0), VOIDmode))
		    continue;
 		  
 		  /* Emit brcc (or brcc_s if possible) */
 		  op1 = XEXP ( SET_SRC (pat), 0);
 		  op2 = XEXP ( SET_SRC (pat), 1);
 		  
 
 		  if ((offset >= -126 && offset <126) 
 		      && immediate_operand (op2, VOIDmode)
 		      && (INTVAL (op2) == 0)
 		      && compact_register_operand (op1, VOIDmode)
 		      && ((GET_CODE (operator) == EQ) || (GET_CODE (operator) == NE)))
 		    brcc_s_possible_rtx = const1_rtx;
 		  else 
 		    brcc_s_possible_rtx = const0_rtx;
 		  
 		  brcc_insn = emit_jump_insn_before (gen_cbranchsi4_scratch 
 						     (gen_rtx_fmt_ee (GET_CODE(operator), GET_MODE(operator), NULL, NULL),
 						      XEXP ( SET_SRC (pat), 0),
 						      XEXP ( SET_SRC (pat), 1),
 						      XEXP (label, 0),
 						      brcc_s_possible_rtx), insn);
 		  JUMP_LABEL (brcc_insn) = JUMP_LABEL (insn);
 		
 		  changed = 1;
 
 		  /* Delete the bcc insn */
 		  PUT_CODE (insn, NOTE);
 		  NOTE_LINE_NUMBER (insn)
 		    = NOTE_INSN_DELETED;
 		  NOTE_SOURCE_FILE (insn) = 0;
 	      
 		  /* Delete the cmp insn */
 		  PUT_CODE (link_insn, NOTE);
 		  NOTE_LINE_NUMBER (link_insn)
 		    = NOTE_INSN_DELETED;
 		  NOTE_SOURCE_FILE (link_insn) = 0;
 		  
 		}
 	      
 	    }
 	}
       /* Clear out insn_addresses */
       INSN_ADDRESSES_FREE ();
 
     }while (changed);
 
   if (INSN_ADDRESSES_SET_P())
       fatal_error ("Insn addresses not freed\n");
   
}
 
 /* Check if the operands are valid for BRcc.d generation
    Valid Brcc.d patterns are
        Brcc.d b, c, s9
        Brcc.d b, u6, s9
        
        For cc={GT, LE, GTU, LEU}, u6=63 can not be allowed, 
      since they are encoded by the assembler as {GE, LT, HS, LS} 64, which 
      does not have a delay slot
 
  Assumed precondition: Second operand is either a register or a u6 value.  */
int
valid_brcc_with_delay_p (rtx * operands)
{
   enum rtx_code operator = GET_CODE (operands[0]); 
 
   if (rtx_equal_p (operands[4], const1_rtx))
     return 0;
 
   if (which_alternative == 0
       && INTVAL (operands[2]) == 63
       && (operator == GT || operator == LE || operator == GTU || operator == LEU))
     return 0;
 
   return 1;
}

/* ??? Hack.  This should no really be here.  See PR32143.  */
static bool
arc_decl_anon_ns_mem_p (tree decl)
{
  while (1)
    {
      if (decl == NULL_TREE || decl == error_mark_node)
        return false;
      if (TREE_CODE (decl) == NAMESPACE_DECL
          && DECL_NAME (decl) == NULL_TREE)
        return true;
      /* Classes and namespaces inside anonymous namespaces have
         TREE_PUBLIC == 0, so we can shortcut the search.  */
      else if (TYPE_P (decl))
        return (TREE_PUBLIC (TYPE_NAME (decl)) == 0);
      else if (TREE_CODE (decl) == NAMESPACE_DECL)
        return (TREE_PUBLIC (decl) == 0);
      else
        decl = DECL_CONTEXT (decl);
    }
}

/* Implement TARGET_IN_SMALL_DATA_P.  Return true if it would be safe to
   access DECL using %gp_rel(...)($gp).  */

static bool
arc_in_small_data_p (tree decl)
{
  HOST_WIDE_INT size;

  if (TARGET_A4)
    return false;

  if (TREE_CODE (decl) == STRING_CST || TREE_CODE (decl) == FUNCTION_DECL)
    return false;


  /* We don't yet generate small-data references for -mabicalls.  See related
     -G handling in override_options.  */
  if (TARGET_NO_SDATA_SET)
    return false;

  if (TREE_CODE (decl) == VAR_DECL && DECL_SECTION_NAME (decl) != 0)
    {
      const char *name;

      /* Reject anything that isn't in a known small-data section.  */
      name = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));
      if (strcmp (name, ".sdata") != 0 && strcmp (name, ".sbss") != 0)
	return false;

      /* If a symbol is defined externally, the assembler will use the
	 usual -G rules when deciding how to implement macros.  */
      if (!DECL_EXTERNAL (decl))
	  return true;
    }
  /* Only global variables go into sdata section for now */
  else if (1)
    {
      /* Don't put constants into the small data section: we want them
	 to be in ROM rather than RAM.  */
      if (TREE_CODE (decl) != VAR_DECL)
	return false;

      if (TREE_READONLY (decl)
	  && !TREE_SIDE_EFFECTS (decl)
	  && (!DECL_INITIAL (decl) || TREE_CONSTANT (DECL_INITIAL (decl))))
	return false;

      /* TREE_PUBLIC might change after the first call, because of the patch
	 for PR19238.  */
      if (default_binds_local_p_1 (decl, 1)
	  || arc_decl_anon_ns_mem_p (decl))
	return false;

      /* To ensure -mvolatile-cache works
	 ld.di does not have a gp-relative variant */
      if (TREE_THIS_VOLATILE (decl))
	return false;
    }

  /* Disable sdata references to weak variables */
  if (DECL_WEAK (decl))
    return false;

  size = int_size_in_bytes (TREE_TYPE (decl));

/*   if (AGGREGATE_TYPE_P (TREE_TYPE (decl))) */
/*     return false; */

  /* Allow only <=4B long data types into sdata */
  return (size > 0 && size <= 4);
}

/* Return true if X is a small data address that can be rewritten
   as a gp+symref.  */

static bool
arc_rewrite_small_data_p (rtx x)
{
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  if (GET_CODE (x) == PLUS)
    {
      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	x = XEXP (x, 0);
    }

  return (GET_CODE (x) ==  SYMBOL_REF
	  && SYMBOL_REF_SMALL_P(x));
}

/* A for_each_rtx callback, used by arc_rewrite_small_data.  */

static int
arc_rewrite_small_data_1 (rtx *loc, void *data ATTRIBUTE_UNUSED)
{
  if (arc_rewrite_small_data_p (*loc))     
    {
      rtx top;

      *loc = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, *loc);
      if (loc == data)
	return -1;
      top = *(rtx*) data;
      if (GET_CODE (top) == MEM && &XEXP (top, 0) == loc)
	; /* OK.  */
      else if (GET_CODE (top) == MEM
	  && GET_CODE (XEXP (top, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (top, 0), 0)) == MULT)
	*loc = force_reg (Pmode, *loc);
      else
	gcc_unreachable ();
      return -1;
    }

  if (GET_CODE (*loc) == PLUS
      && rtx_equal_p (XEXP (*loc, 0), pic_offset_table_rtx))
    return -1;

  return 0;
}

/* If possible, rewrite OP so that it refers to small data using
   explicit relocations.  */

rtx
arc_rewrite_small_data (rtx op)
{
  op = copy_insn (op);
  for_each_rtx (&op, arc_rewrite_small_data_1, &op);
  return op;
}

/* A for_each_rtx callback for small_data_pattern.  */

static int
small_data_pattern_1 (rtx *loc, void *data ATTRIBUTE_UNUSED)
{
  if (GET_CODE (*loc) == PLUS
      && rtx_equal_p (XEXP (*loc, 0), pic_offset_table_rtx))
    return  -1;

  return arc_rewrite_small_data_p (*loc);
}

/* Return true if OP refers to small data symbols directly, not through
   a PLUS.  */

int
small_data_pattern (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return (GET_CODE (op) != SEQUENCE
	  && for_each_rtx (&op, small_data_pattern_1, 0));
}

/* Return true if OP is an acceptable memory operand for ARCompact
   16-bit gp-relative load instructions. 
   op shd look like : [r26, symref@sda]
   i.e. (mem (plus (reg 26) (symref with smalldata flag set))
  */
/* volatile cache option still to be handled */

int
compact_sda_memory_operand (rtx op,enum machine_mode  mode)
{
  rtx addr;
  int size;

  /* Eliminate non-memory operations */
  if (GET_CODE (op) != MEM)
    return 0;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  size = GET_MODE_SIZE (mode);

  /* dword operations really put out 2 instructions, so eliminate them. */ 
  if (size > UNITS_PER_WORD)
    return 0;

  /* Decode the address now.  */
  addr = XEXP (op, 0);

  return LEGITIMATE_SMALL_DATA_ADDRESS_P  (addr);
}

void
arc_asm_output_aligned_decl_local (FILE * stream, tree decl, const char * name, 
				   unsigned HOST_WIDE_INT size,
				   unsigned HOST_WIDE_INT align,
				   unsigned HOST_WIDE_INT globalize_p)
{
  int in_small_data =   arc_in_small_data_p (decl);

  if (in_small_data)
    switch_to_section (get_named_section (NULL, ".sbss", 0));
  /*    named_section (0,".sbss",0); */
  else
    switch_to_section (bss_section);

  if (globalize_p)
    (*targetm.asm_out.globalize_label) (stream, name);

  ASM_OUTPUT_ALIGN (stream, floor_log2 ((align) / BITS_PER_UNIT));
  ASM_OUTPUT_TYPE_DIRECTIVE (stream, name, "object");
  ASM_OUTPUT_SIZE_DIRECTIVE (stream, name, size);
  ASM_OUTPUT_LABEL (stream, name);

  if (size != 0)
    ASM_OUTPUT_SKIP (stream, size);
}


































/* SIMD builtins support */
enum simd_insn_args_type {
  Va_Vb_Vc,
  Va_Vb_rlimm,
  Va_Vb_Ic,
  Va_Vb_u6,
  Va_Vb_u8,
  Va_rlimm_u8,

  Va_Vb,

  void_rlimm,
  void_u6,

  Da_u3_rlimm,
  Da_rlimm_rlimm,

  Va_Ib_u8,
  void_Va_Ib_u8,

  Va_Vb_Ic_u8,
  void_Va_u3_Ib_u8
};

struct builtin_description
{
  enum simd_insn_args_type args_type;
  const enum insn_code     icode;
  const char * const       name;
  const enum arc_builtins  code;
  const enum rtx_code      comparison;
  const unsigned int       flag;
};

static const struct builtin_description arc_simd_builtin_desc_list[] =
{
  /* VVV builtins go first */
#define SIMD_BUILTIN(type,code, string, builtin) \
  { type,CODE_FOR_##code, "__builtin_arc_" string, \
    ARC_SIMD_BUILTIN_##builtin, 0, 0 },

  SIMD_BUILTIN (Va_Vb_Vc,    vaddaw_insn,   "vaddaw",     VADDAW)
  SIMD_BUILTIN (Va_Vb_Vc,     vaddw_insn,    "vaddw",      VADDW)
  SIMD_BUILTIN (Va_Vb_Vc,      vavb_insn,     "vavb",       VAVB)
  SIMD_BUILTIN (Va_Vb_Vc,     vavrb_insn,    "vavrb",      VAVRB)
  SIMD_BUILTIN (Va_Vb_Vc,    vdifaw_insn,   "vdifaw",     VDIFAW)
  SIMD_BUILTIN (Va_Vb_Vc,     vdifw_insn,    "vdifw",      VDIFW)
  SIMD_BUILTIN (Va_Vb_Vc,    vmaxaw_insn,   "vmaxaw",     VMAXAW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmaxw_insn,    "vmaxw",      VMAXW)
  SIMD_BUILTIN (Va_Vb_Vc,    vminaw_insn,   "vminaw",     VMINAW)
  SIMD_BUILTIN (Va_Vb_Vc,     vminw_insn,    "vminw",      VMINW)
  SIMD_BUILTIN (Va_Vb_Vc,    vmulaw_insn,   "vmulaw",     VMULAW)
  SIMD_BUILTIN (Va_Vb_Vc,   vmulfaw_insn,  "vmulfaw",    VMULFAW)
  SIMD_BUILTIN (Va_Vb_Vc,    vmulfw_insn,   "vmulfw",     VMULFW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmulw_insn,    "vmulw",      VMULW)
  SIMD_BUILTIN (Va_Vb_Vc,    vsubaw_insn,   "vsubaw",     VSUBAW)
  SIMD_BUILTIN (Va_Vb_Vc,     vsubw_insn,    "vsubw",      VSUBW)
  SIMD_BUILTIN (Va_Vb_Vc,    vsummw_insn,   "vsummw",     VSUMMW)
  SIMD_BUILTIN (Va_Vb_Vc,      vand_insn,     "vand",       VAND)
  SIMD_BUILTIN (Va_Vb_Vc,    vandaw_insn,   "vandaw",     VANDAW)
  SIMD_BUILTIN (Va_Vb_Vc,      vbic_insn,     "vbic",       VBIC)
  SIMD_BUILTIN (Va_Vb_Vc,    vbicaw_insn,   "vbicaw",     VBICAW)
  SIMD_BUILTIN (Va_Vb_Vc,       vor_insn,      "vor",        VOR)
  SIMD_BUILTIN (Va_Vb_Vc,      vxor_insn,     "vxor",       VXOR)
  SIMD_BUILTIN (Va_Vb_Vc,    vxoraw_insn,   "vxoraw",     VXORAW)
  SIMD_BUILTIN (Va_Vb_Vc,      veqw_insn,     "veqw",       VEQW)
  SIMD_BUILTIN (Va_Vb_Vc,      vlew_insn,     "vlew",       VLEW)
  SIMD_BUILTIN (Va_Vb_Vc,      vltw_insn,     "vltw",       VLTW)
  SIMD_BUILTIN (Va_Vb_Vc,      vnew_insn,     "vnew",       VNEW)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr1aw_insn,   "vmr1aw",     VMR1AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr1w_insn,    "vmr1w",      VMR1W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr2aw_insn,   "vmr2aw",     VMR2AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr2w_insn,    "vmr2w",      VMR2W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr3aw_insn,   "vmr3aw",     VMR3AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr3w_insn,    "vmr3w",      VMR3W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr4aw_insn,   "vmr4aw",     VMR4AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr4w_insn,    "vmr4w",      VMR4W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr5aw_insn,   "vmr5aw",     VMR5AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr5w_insn,    "vmr5w",      VMR5W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr6aw_insn,   "vmr6aw",     VMR6AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr6w_insn,    "vmr6w",      VMR6W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr7aw_insn,   "vmr7aw",     VMR7AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr7w_insn,    "vmr7w",      VMR7W)
  SIMD_BUILTIN (Va_Vb_Vc,      vmrb_insn,     "vmrb",       VMRB)
  SIMD_BUILTIN (Va_Vb_Vc,    vh264f_insn,   "vh264f",     VH264F)
  SIMD_BUILTIN (Va_Vb_Vc,   vh264ft_insn,  "vh264ft",    VH264FT)
  SIMD_BUILTIN (Va_Vb_Vc,   vh264fw_insn,  "vh264fw",    VH264FW)
  SIMD_BUILTIN (Va_Vb_Vc,     vvc1f_insn,    "vvc1f",      VVC1F)
  SIMD_BUILTIN (Va_Vb_Vc,    vvc1ft_insn,   "vvc1ft",     VVC1FT)

  SIMD_BUILTIN (Va_Vb_rlimm,    vbaddw_insn,   "vbaddw",     VBADDW)
  SIMD_BUILTIN (Va_Vb_rlimm,    vbmaxw_insn,   "vbmaxw",     VBMAXW)
  SIMD_BUILTIN (Va_Vb_rlimm,    vbminw_insn,   "vbminw",     VBMINW)
  SIMD_BUILTIN (Va_Vb_rlimm,   vbmulaw_insn,  "vbmulaw",    VBMULAW)
  SIMD_BUILTIN (Va_Vb_rlimm,   vbmulfw_insn,  "vbmulfw",    VBMULFW)
  SIMD_BUILTIN (Va_Vb_rlimm,    vbmulw_insn,   "vbmulw",     VBMULW)
  SIMD_BUILTIN (Va_Vb_rlimm,   vbrsubw_insn,  "vbrsubw",    VBRSUBW)
  SIMD_BUILTIN (Va_Vb_rlimm,    vbsubw_insn,   "vbsubw",     VBSUBW)

  /* Va, Vb, Ic instructions */
  SIMD_BUILTIN (Va_Vb_Ic,        vasrw_insn,    "vasrw",      VASRW) 
  SIMD_BUILTIN (Va_Vb_Ic,         vsr8_insn,     "vsr8",       VSR8) 
  SIMD_BUILTIN (Va_Vb_Ic,       vsr8aw_insn,   "vsr8aw",     VSR8AW) 

  /* Va, Vb, u6 instructions */
  SIMD_BUILTIN (Va_Vb_u6,      vasrrwi_insn,  "vasrrwi",    VASRRWi)
  SIMD_BUILTIN (Va_Vb_u6,     vasrsrwi_insn, "vasrsrwi",   VASRSRWi)
  SIMD_BUILTIN (Va_Vb_u6,       vasrwi_insn,   "vasrwi",     VASRWi)
  SIMD_BUILTIN (Va_Vb_u6,     vasrpwbi_insn, "vasrpwbi",   VASRPWBi)
  SIMD_BUILTIN (Va_Vb_u6,    vasrrpwbi_insn,"vasrrpwbi",  VASRRPWBi)
  SIMD_BUILTIN (Va_Vb_u6,      vsr8awi_insn,  "vsr8awi",    VSR8AWi)
  SIMD_BUILTIN (Va_Vb_u6,        vsr8i_insn,    "vsr8i",      VSR8i)

  /* Va, Vb, u8 (simm) instructions */
  SIMD_BUILTIN (Va_Vb_u8,        vmvaw_insn,    "vmvaw",      VMVAW)
  SIMD_BUILTIN (Va_Vb_u8,         vmvw_insn,     "vmvw",       VMVW)
  SIMD_BUILTIN (Va_Vb_u8,        vmvzw_insn,    "vmvzw",      VMVZW)
  SIMD_BUILTIN (Va_Vb_u8,      vd6tapf_insn,  "vd6tapf",    VD6TAPF)

  /* Va, rlimm, u8 (simm) instructions */
  SIMD_BUILTIN (Va_rlimm_u8,    vmovaw_insn,   "vmovaw",     VMOVAW)
  SIMD_BUILTIN (Va_rlimm_u8,     vmovw_insn,    "vmovw",      VMOVW)
  SIMD_BUILTIN (Va_rlimm_u8,    vmovzw_insn,   "vmovzw",     VMOVZW)

  /* Va, Vb instructions */
  SIMD_BUILTIN (Va_Vb,          vabsaw_insn,   "vabsaw",     VABSAW)
  SIMD_BUILTIN (Va_Vb,           vabsw_insn,    "vabsw",      VABSW)
  SIMD_BUILTIN (Va_Vb,         vaddsuw_insn,  "vaddsuw",    VADDSUW)
  SIMD_BUILTIN (Va_Vb,          vsignw_insn,   "vsignw",     VSIGNW)
  SIMD_BUILTIN (Va_Vb,          vexch1_insn,   "vexch1",     VEXCH1)
  SIMD_BUILTIN (Va_Vb,          vexch2_insn,   "vexch2",     VEXCH2)
  SIMD_BUILTIN (Va_Vb,          vexch4_insn,   "vexch4",     VEXCH4)
  SIMD_BUILTIN (Va_Vb,          vupbaw_insn,   "vupbaw",     VUPBAW)
  SIMD_BUILTIN (Va_Vb,           vupbw_insn,    "vupbw",      VUPBW)
  SIMD_BUILTIN (Va_Vb,         vupsbaw_insn,  "vupsbaw",    VUPSBAW)
  SIMD_BUILTIN (Va_Vb,          vupsbw_insn,   "vupsbw",     VUPSBW)
  
  /* DIb, rlimm, rlimm instructions */
  SIMD_BUILTIN (Da_rlimm_rlimm,  vdirun_insn,  "vdirun",     VDIRUN)
  SIMD_BUILTIN (Da_rlimm_rlimm,  vdorun_insn,  "vdorun",     VDORUN)

  /* DIb, limm, rlimm instructions */
  SIMD_BUILTIN (Da_u3_rlimm,   vdiwr_insn,    "vdiwr",      VDIWR)
  SIMD_BUILTIN (Da_u3_rlimm,    vdowr_insn,    "vdowr",     VDOWR)

  /* rlimm instructions */
  SIMD_BUILTIN (void_rlimm,        vrec_insn,     "vrec",      VREC)
  SIMD_BUILTIN (void_rlimm,        vrun_insn,     "vrun",      VRUN)
  SIMD_BUILTIN (void_rlimm,     vrecrun_insn,  "vrecrun",   VRECRUN)
  SIMD_BUILTIN (void_rlimm,     vendrec_insn,  "vendrec",   VENDREC)

  /* Va, [Ib,u8] instructions */
  SIMD_BUILTIN (Va_Vb_Ic_u8,       vld32wh_insn,  "vld32wh",   VLD32WH)
  SIMD_BUILTIN (Va_Vb_Ic_u8,       vld32wl_insn,  "vld32wl",   VLD32WL)
  SIMD_BUILTIN (Va_Vb_Ic_u8,         vld64_insn,    "vld64",     VLD64)
  SIMD_BUILTIN (Va_Vb_Ic_u8,         vld32_insn,    "vld32",     VLD32)

  SIMD_BUILTIN (Va_Ib_u8,           vld64w_insn,   "vld64w",   VLD64W)
  SIMD_BUILTIN (Va_Ib_u8,           vld128_insn,   "vld128",   VLD128)
  SIMD_BUILTIN (void_Va_Ib_u8,      vst128_insn,   "vst128",   VST128)
  SIMD_BUILTIN (void_Va_Ib_u8,       vst64_insn,    "vst64",    VST64)

  /* Va, [Ib, u8] instructions */
  SIMD_BUILTIN (void_Va_u3_Ib_u8,  vst16_n_insn,  "vst16_n",   VST16_N)
  SIMD_BUILTIN (void_Va_u3_Ib_u8,  vst32_n_insn,  "vst32_n",   VST32_N)

  SIMD_BUILTIN (void_u6,  vinti_insn,  "vinti",   VINTI)
};

static void
arc_init_simd_builtins (void)
{
  int i;
  tree endlink = void_list_node;
  tree V8HI_type_node = build_vector_type_for_mode (intHI_type_node, V8HImode);

  tree v8hi_ftype_v8hi_v8hi
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node, endlink)));
  tree v8hi_ftype_v8hi_int
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, integer_type_node, endlink)));

  tree v8hi_ftype_v8hi_int_int
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, integer_type_node, 
						 tree_cons (NULL_TREE, integer_type_node, endlink))));

  tree void_ftype_v8hi_int_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, integer_type_node, 
						 tree_cons (NULL_TREE, integer_type_node, endlink))));

  tree void_ftype_v8hi_int_int_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, integer_type_node, 
						 tree_cons (NULL_TREE, integer_type_node, 
							    tree_cons (NULL_TREE, integer_type_node, endlink)))));

  tree v8hi_ftype_int_int
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, integer_type_node, endlink)));

  tree void_ftype_int_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, integer_type_node, endlink)));

  tree void_ftype_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, integer_type_node, endlink));

  tree v8hi_ftype_v8hi
    = build_function_type (V8HI_type_node, tree_cons (NULL_TREE, V8HI_type_node,endlink));

  /* These asserts have been introduced to ensure that the order of builtins
     does not get messed up, else the initialization goes wrong */
  gcc_assert (arc_simd_builtin_desc_list [0].args_type == Va_Vb_Vc);
  for (i=0; arc_simd_builtin_desc_list [i].args_type == Va_Vb_Vc; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  v8hi_ftype_v8hi_v8hi, arc_simd_builtin_desc_list [i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb_rlimm);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb_rlimm; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  v8hi_ftype_v8hi_int, arc_simd_builtin_desc_list [i].code);
 
  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb_Ic);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb_Ic; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  v8hi_ftype_v8hi_int, arc_simd_builtin_desc_list [i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb_u6);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb_u6; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  v8hi_ftype_v8hi_int, arc_simd_builtin_desc_list [i].code);
 
  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  v8hi_ftype_v8hi_int, arc_simd_builtin_desc_list [i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_rlimm_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_rlimm_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  v8hi_ftype_int_int, arc_simd_builtin_desc_list [i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  v8hi_ftype_v8hi, arc_simd_builtin_desc_list [i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Da_rlimm_rlimm);
  for (; arc_simd_builtin_desc_list [i].args_type == Da_rlimm_rlimm; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  void_ftype_int_int, arc_simd_builtin_desc_list [i].code);
  
  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Da_u3_rlimm);
  for (; arc_simd_builtin_desc_list [i].args_type == Da_u3_rlimm; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  void_ftype_int_int, arc_simd_builtin_desc_list [i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == void_rlimm);
  for (; arc_simd_builtin_desc_list [i].args_type == void_rlimm; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  void_ftype_int, arc_simd_builtin_desc_list [i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb_Ic_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb_Ic_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  v8hi_ftype_v8hi_int_int, arc_simd_builtin_desc_list [i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Ib_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Ib_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  v8hi_ftype_int_int, arc_simd_builtin_desc_list [i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == void_Va_Ib_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == void_Va_Ib_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  void_ftype_v8hi_int_int, arc_simd_builtin_desc_list [i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == void_Va_u3_Ib_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == void_Va_u3_Ib_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  void_ftype_v8hi_int_int_int, arc_simd_builtin_desc_list [i].code);
  
  gcc_assert (arc_simd_builtin_desc_list [i].args_type == void_u6);
  for (; arc_simd_builtin_desc_list [i].args_type == void_u6; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,  void_ftype_int, arc_simd_builtin_desc_list [i].code);

  gcc_assert(i == ARRAY_SIZE (arc_simd_builtin_desc_list));
}

static rtx
arc_expand_simd_builtin (tree exp,
			 rtx target,
			 rtx subtarget ATTRIBUTE_UNUSED,
			 enum machine_mode mode ATTRIBUTE_UNUSED,
			 int ignore ATTRIBUTE_UNUSED)
{
  tree              fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree              arglist = TREE_OPERAND (exp, 1);
  tree              arg0;
  tree              arg1;
  tree              arg2;
  tree              arg3;
  rtx               op0;
  rtx               op1;
  rtx               op2;
  rtx               op3;
  rtx               op4;
  rtx pat;
  unsigned int         i;
  int               fcode = DECL_FUNCTION_CODE (fndecl);
  int               icode;
  enum machine_mode mode0;
  enum machine_mode mode1;
  enum machine_mode mode2;
  enum machine_mode mode3;
  enum machine_mode mode4;
  const struct builtin_description * d;

  for (i = 0, d = arc_simd_builtin_desc_list; i < ARRAY_SIZE (arc_simd_builtin_desc_list); i++, d++)
    if (d->code == (const enum arc_builtins) fcode)
      break;

  /* We must get an enty here */
  gcc_assert (i < ARRAY_SIZE (arc_simd_builtin_desc_list));

  switch (d->args_type) {
  case Va_Vb_rlimm:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist);
    arg1 = TREE_VALUE (TREE_CHAIN (arglist));
    op0 = expand_expr (arg0, NULL_RTX, V8HImode, 0);
    op1 = expand_expr (arg1, NULL_RTX, SImode, 0);
    
    target = gen_reg_rtx (V8HImode);
    mode0 =  insn_data[icode].operand[1].mode;
    mode1 =  insn_data[icode].operand[2].mode;
    
    if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
      op0 = copy_to_mode_reg (mode0, op0);

    if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);
     
    pat = GEN_FCN (icode) (target, op0, op1);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return target;

  case Va_Vb_u6:
  case Va_Vb_u8:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist);
    arg1 = TREE_VALUE (TREE_CHAIN (arglist));
    op0 = expand_expr (arg0, NULL_RTX, V8HImode, 0);
    op1 = expand_expr (arg1, NULL_RTX, SImode, 0);
    
    target = gen_reg_rtx (V8HImode);
    mode0 =  insn_data[icode].operand[1].mode;
    mode1 =  insn_data[icode].operand[2].mode;
    
    if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
      op0 = copy_to_mode_reg (mode0, op0);

    if ((! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	||  (d->args_type == Va_Vb_u6 && !(UNSIGNED_INT6 (INTVAL (op1))))   
	||  (d->args_type == Va_Vb_u8 && !(UNSIGNED_INT8 (INTVAL (op1))))   
	)
      error ("Operand 2 of %s instruction should be an unsigned %d-bit value.", 
	     d->name,
	     (d->args_type == Va_Vb_u6)? 6: 8);
    
    pat = GEN_FCN (icode) (target, op0, op1);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return target;

  case Va_rlimm_u8:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist);
    arg1 = TREE_VALUE (TREE_CHAIN (arglist));
    op0 = expand_expr (arg0, NULL_RTX, SImode, 0);
    op1 = expand_expr (arg1, NULL_RTX, SImode, 0);
    
    target = gen_reg_rtx (V8HImode);
    mode0 =  insn_data[icode].operand[1].mode;
    mode1 =  insn_data[icode].operand[2].mode;
    
    if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
      op0 = copy_to_mode_reg (mode0, op0);

    if ( (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
	 || !(UNSIGNED_INT8 (INTVAL (op1))))
      error ("Operand 2 of %s instruction should be an unsigned 8-bit value.", 
	     d->name);
    
    pat = GEN_FCN (icode) (target, op0, op1);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return target;

  case Va_Vb_Ic:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist);
    arg1 = TREE_VALUE (TREE_CHAIN (arglist));
    op0 = expand_expr (arg0, NULL_RTX, V8HImode, 0);
    op1 = expand_expr (arg1, NULL_RTX, SImode, 0);
    op2 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);

    target = gen_reg_rtx (V8HImode);
    mode0 =  insn_data[icode].operand[1].mode;
    mode1 =  insn_data[icode].operand[2].mode;
    
    if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
      op0 = copy_to_mode_reg (mode0, op0);

    if ( (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
	 || !(UNSIGNED_INT3 (INTVAL (op1))))
      error ("Operand 2 of %s instruction should be an unsigned 3-bit value (I0-I7).",
	     d->name);
    
    pat = GEN_FCN (icode) (target, op0, op1, op2);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return target;

  case Va_Vb_Vc:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist);
    arg1 = TREE_VALUE (TREE_CHAIN (arglist));
    op0 = expand_expr (arg0, NULL_RTX, V8HImode, 0);
    op1 = expand_expr (arg1, NULL_RTX, V8HImode, 0);
    
    target = gen_reg_rtx (V8HImode);
    mode0 =  insn_data[icode].operand[1].mode;
    mode1 =  insn_data[icode].operand[2].mode;
    
    if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
      op0 = copy_to_mode_reg (mode0, op0);
    
    if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
      op1 = copy_to_mode_reg (mode1, op1);
    
    pat = GEN_FCN (icode) (target, op0, op1);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return target;

  case Va_Vb:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist);
    op0 = expand_expr (arg0, NULL_RTX, V8HImode, 0);
    
    target = gen_reg_rtx (V8HImode);
    mode0 =  insn_data[icode].operand[1].mode;
    
    if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
      op0 = copy_to_mode_reg (mode0, op0);
    
    pat = GEN_FCN (icode) (target, op0);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return target;

  case Da_rlimm_rlimm:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist);
    arg1 = TREE_VALUE (TREE_CHAIN (arglist));
    op0 = expand_expr (arg0, NULL_RTX, SImode, 0);
    op1 = expand_expr (arg1, NULL_RTX, SImode, 0);

    
    if (icode == CODE_FOR_vdirun_insn)
      target = gen_rtx_REG (SImode, 131);
    else if (icode == CODE_FOR_vdorun_insn)
      target = gen_rtx_REG (SImode, 139);
    else
	gcc_unreachable ();

    mode0 =  insn_data[icode].operand[1].mode;
    mode1 =  insn_data[icode].operand[2].mode;
    
    if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
      op0 = copy_to_mode_reg (mode0, op0);

    if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
      op1 = copy_to_mode_reg (mode1, op1);

    
    pat = GEN_FCN (icode) (target, op0, op1);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return NULL_RTX;

  case Da_u3_rlimm:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist);
    arg1 = TREE_VALUE (TREE_CHAIN (arglist));
    op0 = expand_expr (arg0, NULL_RTX, SImode, 0);
    op1 = expand_expr (arg1, NULL_RTX, SImode, 0);

    
    if (! (GET_CODE (op0) == CONST_INT)
	|| !(UNSIGNED_INT3 (INTVAL (op0))))
      error ("Operand 1 of %s instruction should be an unsigned 3-bit value (DR0-DR7).",
	     d->name);
      
    mode1 =  insn_data[icode].operand[1].mode;

    if (icode == CODE_FOR_vdiwr_insn)
      target = gen_rtx_REG (SImode, ARC_FIRST_SIMD_DMA_CONFIG_IN_REG + INTVAL (op0)); 
    else if (icode == CODE_FOR_vdowr_insn)
      target = gen_rtx_REG (SImode, ARC_FIRST_SIMD_DMA_CONFIG_OUT_REG + INTVAL (op0));
    else
      gcc_unreachable ();
    
    if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
      op1 = copy_to_mode_reg (mode1, op1);

    pat = GEN_FCN (icode) (target, op1);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return NULL_RTX;

  case void_u6:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist);
    
    fold (arg0);
    
    op0 = expand_expr (arg0, NULL_RTX, SImode, 0);
    mode0 = insn_data[icode].operand[0].mode;

    /* op0 should be u6 */
    if (! (*insn_data[icode].operand[0].predicate) (op0, mode0)
	|| !(UNSIGNED_INT6 (INTVAL (op0))))
      error ("Operand of %s instruction should be an unsigned 6-bit value.",
	     d->name);
    
    pat = GEN_FCN (icode) (op0);
    if (! pat)
      return 0;
    
    emit_insn (pat);
    return NULL_RTX;
    
  case void_rlimm:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist);
    
    fold (arg0);
    
    op0 = expand_expr (arg0, NULL_RTX, SImode, 0);
    mode0 = insn_data[icode].operand[0].mode;
    
    if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
      op0 = copy_to_mode_reg (mode0, op0);
    
    pat = GEN_FCN (icode) (op0);
    if (! pat)
      return 0;
    
    emit_insn (pat);
    return NULL_RTX;
    
  case Va_Vb_Ic_u8:
    {
      rtx src_vreg;
      icode = d->icode;
      arg0 = TREE_VALUE (arglist); /* source vreg */
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));	/* [I]0-7 */
      arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))); /* u8 */

      src_vreg = expand_expr (arg0, NULL_RTX, V8HImode, 0);
      op0 = expand_expr (arg1, NULL_RTX, SImode, 0);    /* [I]0-7 */
      op1 = expand_expr (arg2, NULL_RTX, SImode, 0);    /* u8 */
      op2 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);	                /* VR0 */
    
      /* target <- src vreg */
      emit_insn (gen_move_insn (target, src_vreg));

      /* target <- vec_concat: target, mem(Ib, u8) */
      mode0 =  insn_data[icode].operand[3].mode;
      mode1 =  insn_data[icode].operand[1].mode;
    
      if ( (!(*insn_data[icode].operand[3].predicate) (op0, mode0))
	   || !(UNSIGNED_INT3 (INTVAL (op0))))
	error ("Operand 1 of %s instruction should be an unsigned 3-bit value (I0-I7).",
	       d->name);

      if ( (!(*insn_data[icode].operand[1].predicate) (op1, mode1))
	   || !(UNSIGNED_INT8 (INTVAL (op1))))
	error ("Operand 2 of %s instruction should be an unsigned 8-bit value.",
	       d->name);
    
      pat = GEN_FCN (icode) (target, op1, op2, op0);
      if (! pat)
	return 0;
  
      emit_insn (pat);
      return target;
    }

  case void_Va_Ib_u8:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist); /* src vreg */
    arg1 = TREE_VALUE (TREE_CHAIN (arglist));	/* [I]0-7 */
    arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))); /* u8 */

    op0 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);  /* VR0    */
    op1 = expand_expr (arg1, NULL_RTX, SImode, 0);        /* I[0-7] */
    op2 = expand_expr (arg2, NULL_RTX, SImode, 0);        /* u8     */
    op3 = expand_expr (arg0, NULL_RTX, V8HImode, 0);      /* Vdest  */
    
    mode0 =  insn_data[icode].operand[0].mode;
    mode1 =  insn_data[icode].operand[1].mode;
    mode2 =  insn_data[icode].operand[2].mode;
    mode3 =  insn_data[icode].operand[3].mode;
    
    if ( (!(*insn_data[icode].operand[1].predicate) (op1, mode1))
	 || !(UNSIGNED_INT3 (INTVAL (op1))))
      error ("Operand 2 of %s instruction should be an unsigned 3-bit value (I0-I7).",
	     d->name);

    if ( (!(*insn_data[icode].operand[2].predicate) (op2, mode2))
	 || !(UNSIGNED_INT8 (INTVAL (op2))))
      error ("Operand 3 of %s instruction should be an unsigned 8-bit value.",
	     d->name);
    
    if (!(*insn_data[icode].operand[3].predicate) (op3, mode3))
      op3 = copy_to_mode_reg (mode3, op3);
      
    pat = GEN_FCN (icode) (op0, op1, op2, op3);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return NULL_RTX;

  case Va_Ib_u8:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist); /* dest vreg */
    arg1 = TREE_VALUE (TREE_CHAIN (arglist));	/* [I]0-7 */

    op0 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);  /* VR0    */
    op1 = expand_expr (arg0, NULL_RTX, SImode, 0);        /* I[0-7] */
    op2 = expand_expr (arg1, NULL_RTX, SImode, 0);        /* u8     */
    
    /* target <- src vreg */
    target = gen_reg_rtx (V8HImode);

    /* target <- vec_concat: target, mem(Ib, u8) */
    mode0 =  insn_data[icode].operand[1].mode;
    mode1 =  insn_data[icode].operand[2].mode;
    mode2 =  insn_data[icode].operand[3].mode;
    
    if ( (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
	 || !(UNSIGNED_INT3 (INTVAL (op1))))
      error ("Operand 1 of %s instruction should be an unsigned 3-bit value (I0-I7).",
	     d->name);

    if ( (!(*insn_data[icode].operand[3].predicate) (op2, mode2))
	 || !(UNSIGNED_INT8 (INTVAL (op2))))
      error ("Operand 2 of %s instruction should be an unsigned 8-bit value.",
	     d->name);
    
    pat = GEN_FCN (icode) (target, op0, op1, op2);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return target;

  case void_Va_u3_Ib_u8:
    icode = d->icode;
    arg0 = TREE_VALUE (arglist); /* source vreg */
    arg1 = TREE_VALUE (TREE_CHAIN (arglist));	/* u3 */
    arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))); /* [I]0-7 */
    arg3 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (arglist)))); /* u8 */

    op0 = expand_expr (arg3, NULL_RTX, SImode, 0);         /* u8               */
    op1 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);  /* VR                */
    op2 = expand_expr (arg2, NULL_RTX, SImode, 0);        /* [I]0-7            */
    op3 = expand_expr (arg0, NULL_RTX, V8HImode, 0);      /* vreg to be stored */
    op4 = expand_expr (arg1, NULL_RTX, SImode, 0);        /* vreg 0-7 subreg no. */

    mode0 =  insn_data[icode].operand[0].mode;
    mode2 =  insn_data[icode].operand[2].mode;
    mode3 =  insn_data[icode].operand[3].mode;
    mode4 =  insn_data[icode].operand[4].mode;
    
    /* correctness checks for the operands */
    if ( (!(*insn_data[icode].operand[0].predicate) (op0, mode0))
	 || !(UNSIGNED_INT8 (INTVAL (op0))))
      error ("Operand 4 of %s instruction should be an unsigned 8-bit value (0-255).",
	     d->name);

    if ( (!(*insn_data[icode].operand[2].predicate) (op2, mode2))
	 || !(UNSIGNED_INT3 (INTVAL (op2))))
      error ("Operand 3 of %s instruction should be an unsigned 3-bit value (I0-I7).",
	     d->name);

    if (!(*insn_data[icode].operand[3].predicate) (op3, mode3))
      op3 = copy_to_mode_reg (mode3, op3);
      
    if ( (!(*insn_data[icode].operand[4].predicate) (op4, mode4))
	 || !(UNSIGNED_INT3 (INTVAL (op4))))
      error ("Operand 2 of %s instruction should be an unsigned 3-bit value (subreg 0-7).",
	     d->name);
    else if (icode == CODE_FOR_vst32_n_insn 
	     && ((INTVAL(op4) % 2 ) != 0))
      error ("Operand 2 of %s instruction should be an even 3-bit value (subreg 0,2,4,6).",
	     d->name);

    pat = GEN_FCN (icode) (op0, op1, op2, op3, op4);
    if (! pat)
      return 0;
  
    emit_insn (pat);
    return NULL_RTX;

  default:
    gcc_unreachable ();
  }
  return NULL_RTX;
}

enum reg_class
arc_secondary_reload (bool in_p, rtx x, enum reg_class class,
                     enum machine_mode mode ATTRIBUTE_UNUSED,
		     secondary_reload_info *sri ATTRIBUTE_UNUSED)
{
  /* We can't load/store the D-registers directly */
  if (class == DOUBLE_REGS && (GET_CODE (x) == MEM))
    return GENERAL_REGS;
  /* The loop counter register can be stored, but not loaded directly.  */
  if (class == LPCOUNT_REG && in_p)
    return GENERAL_REGS;
  return NO_REGS;
}

static bool
arc_preserve_reload_p (rtx in)
{
  return (GET_CODE (in) == PLUS
	  && RTX_OK_FOR_BASE_P (XEXP (in, 0))
	  && CONST_INT_P (XEXP (in, 1))
	  && !((INTVAL (XEXP (in, 1)) & 511)));
}

int
arc_register_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
			enum reg_class class1 ATTRIBUTE_UNUSED,
			enum reg_class class2 ATTRIBUTE_UNUSED)
{
  return 2;
}

const char*
arc_output_addsi (rtx *operands, const char *cond)
{
  char format[32];
  
  int cond_p = *cond;
  int match = operands_match_p (operands[0], operands[1]);
  int intval = (REG_P (operands[2]) ? 1
		: CONST_INT_P (operands[2]) ? INTVAL (operands[2]) : 0xbadc057);
  int neg_intval = -intval;
  int shift = 0;
  int short_p = 0;

  /* First try to emit a 16 bit insn.  */
  if (arc_size_opt_level >= 1)
    {
      int short_0 = satisfies_constraint_Rcq (operands[0]);

      short_p = (!cond_p && short_0 && satisfies_constraint_Rcq (operands[1]));
      if (short_p
	  && (REG_P (operands[2])
	      ? (match || satisfies_constraint_Rcq (operands[2]))
	      : (unsigned) intval <= (match ? 127 : 7)))
	return "add_s %0,%1,%2";
      if (!cond_p && (short_0 || REGNO (operands[0]) == STACK_POINTER_REGNUM)
	  && REGNO (operands[1]) == STACK_POINTER_REGNUM && !(intval & ~124))
	return "add_s %0,%1,%2";

      if ((short_p && (unsigned) neg_intval <= (match ? 31 : 7))
	  || (!cond_p && REGNO (operands[0]) == STACK_POINTER_REGNUM
	      && match && !(intval & ~124)))
	return "sub_s %0,%1,%n2";
    }

  /* Now try to emit a 32 bit insn without long immediate.  */
  if (match || !cond_p)
    {
      int limit = (match && !cond_p) ? 0x7ff : 0x3f;
      int range_factor = neg_intval & intval;

      if (intval == -1 << 31)
	{
	  sprintf (format, "bxor%s %%0,%%1,31", cond);
	  output_asm_insn (format, operands);
	  return "";
	}

      /* If we can use a straight add / sub instead of a {add,sub}[123] of
	 same size, do, so - the insn latency is lower.  */
      /* -0x800 is a 12-bit constant for add /add3 / sub / sub3, but
	 0x800 is not.  */
      if ((intval >= 0 && intval <= limit)
	       || (intval == -0x800 && limit == 0x7ff))
	{
	  sprintf (format, "add%s %%0,%%1,%%2", cond);
	  output_asm_insn (format, operands);
	  return "";
	}
      else if ((intval < 0 && neg_intval <= limit)
	       || (intval == 0x800 && limit == 0x7ff))
	{
	  sprintf (format, "sub%s %%0,%%1,%%n2", cond);
	  output_asm_insn (format, operands);
	  return "";
	}
      shift = range_factor >= 8 ? 3 : (range_factor >> 1 & 3);
      if (((intval < 0 && intval != -0x4000)
	   /* sub[123] is slower than add_s / sub, only use it if it
	      avoids a long immediate.  */
	   && neg_intval <= limit << shift)
	  || (intval == 0x4000 && limit == 0x7ff))
	{
	  sprintf (format, "sub%d%s %%0,%%1,%d", shift, cond,
		   neg_intval >> shift);
	  output_asm_insn (format, operands);
	  return "";
	}
      else if ((intval >= 0 && intval <= limit << shift)
	       || (intval == -0x4000 && limit == 0x7ff))
	{
	  sprintf (format, "add%d%s %%0,%%1,%d", shift, cond, intval >> shift);
	  output_asm_insn (format, operands);
	  return "";
	}
    }
  /* Try to emit a 16 bit opcode with long immediate.  */
  if (short_p && match)
    return "add_s %0,%1,%S2"; 

  /* We have to use a 32 bit opcode with a long immediate.  */
  sprintf (format, intval < 0 ? "sub%s %%0,%%1,%%n2" : "add%s %%0,%%1,%%S2",
	   cond);
  output_asm_insn (format, operands);
  return "";
}

static rtx
force_offsettable (rtx addr, HOST_WIDE_INT size, int reuse)
{
  rtx base = addr;
  rtx offs = const0_rtx;

  if (GET_CODE (base) == PLUS)
    {
      offs = XEXP (base, 1);
      base = XEXP (base, 0);
    }
  if (!REG_P (base)
      || (REGNO (base) != STACK_POINTER_REGNUM
	  && REGNO_PTR_FRAME_P (REGNO (addr)))
      || !CONST_INT_P (offs) || !SMALL_INT (INTVAL (offs))
      || !SMALL_INT (INTVAL (offs) + size))
    {
      if (reuse)
	emit_insn (gen_add2_insn (addr, offs));
      else
	addr = copy_to_mode_reg (Pmode, addr);
    }
  return addr;
}

/* Like move_by_pieces, but take account of load latency,
   and actual offset ranges.
   Return nonzero on success.  */
int
arc_expand_movmem (rtx *operands)
{
  rtx dst = operands[0];
  rtx src = operands[1];
  rtx dst_addr, src_addr;
  HOST_WIDE_INT size;
  int align = INTVAL (operands[3]);
  unsigned n_pieces;
  int piece = align;
  rtx store[2];
  rtx tmpx[2];
  int i;

  if (!CONST_INT_P (operands[2]))
    return 0;
  size = INTVAL (operands[2]);
  /* move_by_pieces_ninsns is static, so we can't use it.  */
  if (align >= 4)
    n_pieces = (size + 2) / 4U + (size & 1);
  else if (align == 2)
    n_pieces = (size + 1) / 2U;
  else
    n_pieces = size;
  if (n_pieces >= (unsigned int) (optimize_size ? 3 : 15))
    return 0;
  if (piece > 4)
    piece = 4;
  dst_addr = force_offsettable (XEXP (operands[0], 0), size, 0);
  src_addr = force_offsettable (XEXP (operands[1], 0), size, 0);
  store[0] = store[1] = NULL_RTX;
  tmpx[0] = tmpx[1] = NULL_RTX;
  for (i = 0; size > 0; i ^= 1, size -= piece)
    {
      rtx tmp;
      enum machine_mode mode;

      if (piece > size)
	piece = size & -size;
      mode = smallest_mode_for_size (piece * BITS_PER_UNIT, MODE_INT);
      /* If we don't re-use temporaries, the scheduler gets carried away,
	 and the register pressure gets unnecessarily high.  */
      if (0 && tmpx[i] && GET_MODE (tmpx[i]) == mode)
	tmp = tmpx[i];
      else
	tmpx[i] = tmp = gen_reg_rtx (mode);
      dst_addr = force_offsettable (dst_addr, piece, 1);
      src_addr = force_offsettable (src_addr, piece, 1);
      if (store[i])
	emit_insn (store[i]);
      emit_move_insn (tmp, change_address (src, mode, src_addr));
      store[i] = gen_move_insn (change_address (dst, mode, dst_addr), tmp);
      dst_addr = plus_constant (dst_addr, piece);
      src_addr = plus_constant (src_addr, piece);
    }
  if (store[i])
    emit_insn (store[i]);
  if (store[i^1])
    emit_insn (store[i^1]);
  return 1;
}

/* Prepare operands for move in MODE.  Return nonzero iff the move has
   been emitted.  */
int
prepare_move_operands (rtx *operands, enum machine_mode mode)
{
  /* We used to do this only for MODE_INT Modes, but addresses to floating
     point variables may well be in the small data section.  */
  if (1)
    {
      if (!TARGET_NO_SDATA_SET && small_data_pattern (operands[0], Pmode))
	operands[0] = arc_rewrite_small_data (operands[0]);
      else if (mode == SImode && flag_pic && SYMBOLIC_CONST (operands[1]))
	{
	  emit_pic_move (operands, SImode);

	  /* Disable any REG_EQUALs associated with the symref
	     otherwise the optimization pass undoes the work done
	     here and references the variable directly.  */
	}
      else if (GET_CODE (operands[0]) != MEM
	       && !TARGET_NO_SDATA_SET
	       && small_data_pattern (operands[1], Pmode))
       {
	  /* This is to take care of address calculations involving sdata
	     variables.  */
	  operands[1] = arc_rewrite_small_data (operands[1]);

	  emit_insn (gen_rtx_SET (mode, operands[0],operands[1]));
	  /* ??? This note is useless, since it only restates the set itself.
	     We should rather use the original SYMBOL_REF.  However, there is
	     the problem that we are lying to the compiler about these
	     SYMBOL_REFs to start with.  symbol@sda should be encoded specially
	     so that we can tell it apart from an actual symbol.  */
	  set_unique_reg_note (get_last_insn (), REG_EQUAL, operands[1]);

	  /* Take care of the REG_EQUAL note that will be attached to mark the
	     output reg equal to the initial symbol_ref after this code is
	     executed. */
	  emit_move_insn (operands[0], operands[0]);
	  return 1;
	}
    }

  if (GET_CODE (operands[0]) == MEM
      && !(reload_in_progress || reload_completed))
    {
      operands[1] = force_reg (mode, operands[1]);
      if (!move_dest_operand (operands[0], mode))
	{
	  rtx addr = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
	  /* This is like change_address_1 (operands[0], mode, 0, 1) ,
	     except that we can't use that function because it is static.  */
	  rtx new = change_address (operands[0], mode, addr);
	  MEM_COPY_ATTRIBUTES (new, operands[0]);
	  operands[0] = new;
	}
    }
  return 0;
}

/* Prepare OPERANDS for an extension using CODE to OMODE.
   Return nonzero iff the move has been emitted.  */
int
prepare_extend_operands (rtx *operands, enum rtx_code code,
			 enum machine_mode omode)
{
  if (!TARGET_NO_SDATA_SET && small_data_pattern (operands[1], Pmode))
    {
      /* This is to take care of address calculations involving sdata
	 variables.  */
      operands[1]
	= gen_rtx_fmt_e (code, omode, arc_rewrite_small_data (operands[1]));
      emit_insn (gen_rtx_SET (omode, operands[0], operands[1]));
      set_unique_reg_note (get_last_insn (), REG_EQUAL, operands[1]);

      /* Take care of the REG_EQUAL note that will be attached to mark the 
	 output reg equal to the initial extension after this code is 
	 executed. */
      emit_move_insn (operands[0], operands[0]);
      return 1;
    }
  return 0;
}

const char *
arc_output_libcall (const char *fname)
{
  unsigned len = strlen (fname);
  static char buf[64];

  gcc_assert (len < sizeof buf - 6);
  if (TARGET_LONG_CALLS_SET)
    {
      if (flag_pic)
	sprintf (buf, "add r12,pcl,@%s-(.&2)\n\tjl%%* r12", fname);
      else
	sprintf (buf, "jl%%? @%s", fname);
    }
  else
    sprintf (buf, "bl%%* @%s", fname);
  return buf;
}

static rtx
arc_delegitimize_address (rtx x)
{
  if (GET_CODE (x) == MEM && GET_CODE (XEXP (x, 0)) == CONST
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == UNSPEC
      && XINT (XEXP (XEXP (x, 0), 0), 1) == ARC_UNSPEC_GOT)
    return XVECEXP (XEXP (XEXP (x, 0), 0), 0, 0);
  return x;
}
