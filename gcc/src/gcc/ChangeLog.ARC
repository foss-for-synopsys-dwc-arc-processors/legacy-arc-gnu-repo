2011-01-20  Brendan Kehoe  <brendan@zen.org>

	CR 101612
	* config/arc/ieee-754/adddf3.S (Linf_nan): Fix to give NaN when
	the signs on values of infinity don't match.
	* config/arc/ieee-754/muldf3.S (Linf_nan): Fix to honor inf*0.0
	and properly give back NaN.

2010-03-15  Brendan Kehoe  <brendan@zen.org>

	CR 98782
	* config/arc/arc.md (cbranchsi4_scratch): Include a clobber of
	the CC reg 61.
	* config/arc/arc.c (arc_reorg): Pass REG 61 as the new final arg
	to gen_cbranchsi4_scratch.

2010-02-08  Brendan Kehoe  <brendan@zen.org>

	Restrict control of the LP_COUNT register to the MULT insn
	specifically, not all commutative operators.
	* config/arc/arc.md (*commutative_binary_comparison_result_used): Revert to 'w' constraint, but change the predicate to commutative_operator_sans_mult.
	(*commutative_binary_mult_comparison_result_used): New pattern similar to the other, but matching the mult_operand specifically, so we can use the 'W' constraint.
	* config/arc/predicates.md (commutative_operator_sans_mult):
	Copy of commutative_operator, without the "mult" TARGET_ARC700 insn.
	(mult_operator): New predicate.

2010-01-29  Brendan Kehoe  <brendan@zen.org>

	* config/arc/arc.c (arc_conditional_register_usage): If
	-ffixed-lp_count, make sure LP_COUNT is not available in any
	register class, nor does it have any acceptable regno mode.

2010-01-27  Brendan Kehoe  <brendan.kehoe@viragelogic.com>

	* config/arc/arc.md ("*commutative_binary_comparison_result_used"): 
	Use the 'W' constraint since MULT is one of the possible
	commutative operators.
	* config/arc/fpx.md: In both define_peephole2 using
	arc_dpfp_operator, specify "W" register class for the destination
	operand.

2010-01-22  Brendan Kehoe  <brendan.kehoe@viragelogic.com>

	* config/arc/constraints.md (MPY_WRITABLE_CORE_REGS): Define for 'W'.
	* config/arc/arc.h (enum reg_class): Add MPY_WRITABLE_CORE_REGS.
	(REG_CLASS_NAMES): Add "MPY_WRITABLE_CORE_REGS".
	(REG_CLASS_CONTENTS): Define similar to 'w' but with the bit for
	register 60 changed to 0.
	* config/arc/arc.md (mulsi3_700): Change constraint from 'w' to 'W'.
	Adjust each instruction's line to have a more helpful comment.
	* config/arc/predicates.md (mpy_dest_reg_operand): Define using
	MPY_WRITABLE_CORE_REGS as its bounds to make sure it cannot
	use LP_COUNT.

2008-10-10  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (*add_n): Fix length of c/c/Cal alternative.

2008-09-24  Muhammad Khurram Riaz <khurram.riaz@arc.com>

	* config/arc/predicates.md: (move_dest_operand) for MUL64 target
	r57-r59 cannot be destination, the manual says they are read only.
	similarly r56-r57 for target MUL32x16.

2008-09-19  Muhammad Khurram Riaz <khurram.riaz@arc.com>

	* config/arc/arc.md : 
	(mulsi3): for MULMAC32x16 unsigned range can be upto 65535, and 
	mululw in enough (alongwith mov from acc2) to get the result (no
	need for use mac instruction) ; note that single mul instruction can not 
	be used with immediates less then 0 because 
	mululw 0,reg,limm ( and also mullw ) treats lower 16bits of imm operand 
	as unsigned, So in this case mac instruction is also required.
	(umul_600) : limm can be the last operand in dsp-mul instructions
	(mac_600, mul64_600, mac64_600, umul64_600, umac64_600) : Likewise

2008-09-15  Muhammad Khurram Riaz <khurram.riaz@arc.com>

	* config/arc/arc.md : Bug fix in mul/mac instructions
	* config/arc/arc.c : Bug fix in mul/mac instructions

2008-09-12  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_compute_millicode_save_restore_regs): Only
	consider save sequences starting with r13.

2008-09-05  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (doloop_fallback): Operand 0 is "+r,!w".
	(doloop_fallback_m): Operand 0 is "+&r".

2008-09-02  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (ASM_SPEC): For -mmmul32x16, pass -mdsp.

	* config/arc/lib1funcs.asm (__mulsi3): Fix invalid insn issues.

	* config/arc/arc.md (*call_via_label): Replace %^%Pn with %Pn.
	(call_prof, *call_value_via_label, call_value_prof): Likewise.
	(*sibcall_insn, *sibcall_value_insn, sibcall_prof): Likewise.
	(sibcall_value_prof): Likewise.

	* config/arc/arc.md (doloop_begin_i): When n_insns is 0, output
	three nops for ARC600.

	* config/arc/predicates.md (arc_double_register_operand): Fix
	indentation.
	(vector_register_operand, vector_register_or_memory_operand): Likewise.
	(arc_dpfp_operator): Likewise.
	(acc1_operand, acc2_operand, mlo_operand, mhi_operand): New predicate.
	* config/arc/arc.c (arc_init): For ARC600 with TARGET_MUL64_SET,
	default multcost to 4.
	(rname56, rname57, rname58, rname59): New character arrays.
	(arc_init_regno_reg_class): Rename to:
	(arc_conditional_register_usage).  Add code from
	arc.h:CONDITIONAL_REGISTER_USAGE.  Establish special multiply result
	register names, and use register numbers that work well with
	the target endianness.
	For ARC700, disable use of lp_count for SFmode.
	(gen_acc1, gen-acc2, gen_mlo, gen_mhi): New functions.
	* config/arc/arc-protos.h (arc_init_regno_reg_class): Don't declare.
	(arc_conditional_register_usage): Declare.
	(gen_acc1, gen-acc2, gen_mlo, gen_mhi): Likewise.
	* config/arc/arc.h (CONDITIONAL_REGISTER_USAGE): Use
	arc_conditional_register_usage.
	(rname56, rname57, rname58, rname59): Declare.
	(REGISTER_NAMES): Use them.
	(DBX_REGISTER_NUMBER): Translate internal numbers for multiply
	result registers into true hardware register numbers.
	* config/arc/arc.md

	* testsuite/gcc.c-torture/execute/ieee/denorm-rand.c (main):
	Reduce iteration counts to 1000.
	* varasm.c (pickle_in_section): Make defintion a declaration.
	* config/arc/arc.c (arc_print_operand): Add 'o' to output
	symbol without '@'.
	(write_profile_sections): Use it.

	* config/arc/arc.c (arc_final_prescan_insn): Call
	extract_constrain_insn_cached after calling arc_hazard.
	(arc_reorg): Check second parameter of compare before feeding it
	to cbranchsi4_scratch.
	* config/arc/arc.md (*unary_comparison_result_used): Use higher
	operand numbers for match_operator than for ordinary operands.
	Use match_dup for duplicated expression in match_operator.
	(*commutative_binary_comparison_result_used): Likewise.
	(*noncommutative_binary_comparison_result_used): Likewise.
	(mulsi3): Generate expansion for TARGET_MUL64_SET.
	(mulsi3): Refer to multiply result registers with special generator
	/ recognizer functions to account for the endian-dependent numbers.
	(umul_600, smul_600, mac_600, mulsi_600, mulsidi3): Likewise.
	(umulsidi3): Likewise.
	(mulsi_600): Type multi.
	(*split_mulsi3_600): Delete.
	(mulsidi_600, umulsidi_600): New patterns.
	(mulsidi3, umulsidi3): Result is nonimmediate_operand.
	Add TARGET_MUL64_SET code.
	(mac64_600): Fix semantics description to be consistent with
	PARALLEL semantics.
	(umulsidi3_700): Operand 2 is register_operand.
	* config/arc/arc600.md (mul64_600): New cpu_unit.
	(load_DI_600, load_600): Fix attribute test.
	(mul_600_fast, mul_600_slow): New reservations.
	* config/arc/arc.h (TARGET_OPTFPE): True for TARGET_ARC600
	&& TARGET_NORM_SET && TARGET_MULMAC_32BY16_SET.

	* config/arc/arc.md (doloop_begin_i): Look inside SEQUENCEs.
	(doloop_end_i) Change alternative 2 of operand 2 to C_0.
	* config/arc/arc.h (HARD_REGNO_RENAME_OK): Delete.

	* config/arc/predicates.md (dest_reg_operand): New predicate.

2008-09-02  Muhammad Khurram Riaz <khurram.riaz@arc.com>

	* config/arc/arc.c (arc600_corereg_hazard): mul/mac and mul64 set the accum registers 
	and are not a hazard.
	* config/arc/arc.md :
	(*unary_comparison_result_used): added for flag setting instructions(unary) that also use result.
	(*addsi_compare): added for add.f ; needed by combiner.
	(*commutative_binary_comparison_result_used): added for flag setting instructions(binary,commutative) 
	that also use result.
	(*noncommutative_binary_comparison_result_used): added for flag setting instructions(binary,commutative) 
	that also use result.

2008-09-01  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/ieee-754/fixdfsi.S: Fix negative case.

2008-08-30  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c: Give up on zero overhead loop if loop setup
	precedes loop end with no label in-between.

2008-08-30  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_final_prescan_insn): Don't try to eliminate
	'insns' of TYPE_LOOP_END.
	* config/arc/arc.md (attr type): Replace loop with loop_start and
	loop_end.
	(attr in_delay_slot): Update.
	(movqi_insn, movhi_insn): Add alternatives to read ALL_CORE_REGS.
	(doloop_begin_i): Better estimate insn fetches with n_insns.
	Allow zero n_insns if there is a code_label.
	Don't emit nops for ARC600 with loop_top if n_insn is at least 3.
	Change type to loop_setup.
	(doloop_end_i): Set type to loop_end.

2008-08-30  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (doloop_begin_i): Start insn counting with label
	rather than label_ref.  Check that jump sucessor insns exists before
	calling recog_memoized on it.

2008-08-30  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (doloop_begin_i): Do separate calculation of
	minimum insns from loop setup to loop end.
	* config/arc/arc.c (arc_reorg): Fix setting of insn after deletion.

2008-08-29  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_reorg): Use the loop top label to check for
	empty loops.  When deleting an empty loop, put the set of the loop
	count before the loop end.

2008-08-29  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (HARD_REGNO_RENAME_OK): Prohibit renaming from
	LP_COUNT.
	* config/arc/arc.md (doloop_begin_i): Change alternative 2 of
	operand 1 to C_0.

2008-08-29  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (CONDITIONAL_REGISTER_USAGE): If any of the
	registers in the range 32..59 has been made non-fixed, include
	them in WRITABLE_CORE_REGS.
	(enum reg_class, REG_CLASS_NAMES): Add WRITABLE_CORE_REGS.
	(REG_CLASS_CONTENTS): Likewise.
	(PREFERRED_RELOAD_CLASS): Check for WRITABLE_CORE_REGS.

	* config/arc/arc.c (arc_register_move_cost): Express costs of
	writing / reading LP_COUNT register.
	* config/arc/arc.h (CONDITIONAL_REGISTER_USAGE): Prune CHEAP_CORE_REGS.
	(enum reg_class, REG_CLASS_NAMES, REG_CLASS_CONTENTS): Split CORE_REGS
	into CHEAP_CORE_REGS and ALL_CORE_REGS.
	(PREFERRED_RELOAD_CLASS): Check for CHEAP_CORE_REGS.
	(REGISTER_MOVE_COST): Use arc_register_move_cost.
	* config/arc/arc.md (movsi_insn): Add alternatives to read
	ALL_CORE_REGS.
	* config/arc/constraints.md ("c"): Now for CHEAP_CORE_REGS.
	("Rac"): New constraint.

2008-08-29  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_final_prescan_insn): Output a nop if necessary
	to prevent a hazard.
	(arc_output_mi_thunk): Formatting fixes.
	(arc_secondary_reload): Also require temporary
	GENERAL_REGS if loading WRITABLE_CORE_REGS.
	(arc600_corereg_hazard_1, arc600_corereg_hazard): New functions.
	(arc_hazard, arc_insn_length_adjustment): Likewise.
	(write_ext_corereg_1, arc_write_ext_corereg): Likewise.
	* config/arc/arc.h (CONDITIONAL_REGISTER_USAGE):  For ARC600, merely
	remove LP_COUNT from SIBCALL_REGS.
	(REGISTER_MOVE_COST): Increase for LPCOUNT_REG / WRUTABLE_CORE_REGS
	on ARC600.
	(MOVE_RATIO): Remove cast.
	(ADJUST_INSN_LENGTH): Define.
	(IS_ASM_LOGICAL_LINE_SEPARATOR): Define.
	* config/arc/arc.md (in_delay_slot): Take hazards into account.
	* config/arc/arc-protos.h (arc_insn_length_adjustment): Declare.
	(arc_corereg_hazard, arc_hazard, arc_write_ext_corereg): Likewise.
	* config/arc/constraints.md ("w"): Use WRITABLE_CORE_REGS.

	* config/arc/arc.h (WRITABLE_CORE_REGS): Stub definition.

	* loop-doloop.c (doloop_modify): Pass doloop_end pattern to
	gen_doloop_begin.
	* config/c4x/c4x.md (doloop_begin): Operand 4 is doloop_end pattern.
	* loop-doloop.c (doloop_optimize): Pass flag to indicate if loop is
	entered at top to gen_doloop_end.
	* config/arc/arc.c (arc_reorg): If we can't find the loop entry at
	the loop top, search entire function for a matching loop begin,
	and record information about it if found.
	* config/arc/arc.md (doloop_begin): Now takes five operands.
	(doloop_begin_i): Likewise.
	(doloop_end): Now takes 6 operands.

2008-08-27  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/ieee-754/fixdfsi.S: Fix shift by zero case.

2008-08-27  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_final_prescan_insn): Move code after
	declarations.

	* config/arc/arc.md (umulsi3_highpart_i, umulsi3_highpart):
	New patterns.

	* constraints.md ("w"): Stub constraint.

2008-08-27  J"orn Rennecke  <joern.rennecke@arc.com>

	* target-def.h (TARGET_PRESERVE_RELOAD_P): Define.
	(TARGET_INITIALIZER): Add TARGET_PRESERVE_RELOAD_P.
	* target.h (struct gcc_target): New member preserve_reload_p.
	* reload.c (push_reload): Mark values flagged by
	targetm.preserve_reload_p as RELOAD_OTHER.
	* config/arc/arc.c (arc_preserve_reload_p): New function.
	(TARGET_PRESERVE_RELOAD_P): Redefine.
	* doc/tm.texi (TARGET_PRESERVE_RELOAD_P): Document.

2008-08-08  Muhammad Khurram Riaz <khurram.riaz@arc.com>

	* config/arc/predicates.md (move_dest_operand): r56 r57 cannot be destination
	operand of move.
	* config/arc/arc.c (arc_init): error for -mmul32x16 used for !ARC600.
	* config/arc/arc.h (ASM_SPEC): mmul32x16 passed.
	* config/arc/arc.md (mulsi3):  added Arc600 dsp mul/mac also modified for mul64
	(mulsidi3): added Arc600 mul/mac dsp instructions
	* config/arc/arc600.md: scheduling info for new dsp mul/mac intructions.
	* config/arc/arc.opt: new option -mmul32x16

2008-08-08  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (doloop_begin_i): Count asm insns only as a
	single insn.

2008-08-01  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (ASM_OUTPUT_SYMBOL_REF): Define.
	(ASM_OUTPUT_LABEL_REF): Likewise.
	* config/arc/arc.c (arc_assemble_integer, arc_print_operand):
	Don't emit '@' before labels / symbols.
	(arc_print_operand_address): Likewise.

2008-07-29  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (LINK_COMMAND_SPEC): When creating a shared
	library with -nostdlib, add -lgcc_s.

2008-07-29  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/fp-hack.c: Disable debug functions for ARC700.
	* config/arc/dp-hack.c: Likewise.

2008-07-28  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/t-arc700-uClibc (SHLIB_LINK): Don't use SHLIB_LC.

	* config/arc/arc.h (STATIC_LINK_SPEC): Define, making default to
	use dynamic libraries where available.
	(LINK_SPEC): Use STATIC_LINK_SPEC.
	* config.gcc (arc*-*-linux-uclibc*): Retain OS specific files in
	tmake_file.
	* config/arc/t-arc700-uClibc (SHLIB_MAPFILES): Fix pasto.
	(SHLIB_LINK: Override to use a linker script libgcc_s.so.
	(SHLIB_INSTALL): Likewise.
	* config/arc/lib1funcs.asm (__mulsi3): Use (HIDDEN_)FUNC / ENDFUNC.
	(__udivmodsi4, __umodsi3, __modsi3, __clzsi2): Likewise.
	(__umulsidi3): Likewise.  Add ARC700 optimized code.
	* config/arc/ieee-754/adddf3.S: Use (HIDDEN_)FUNC / ENDFUNC.
	* config/arc/ieee-754/muldf3.S: Likewise.

2008-07-25  J"orn Rennecke  <joern.rennecke@arc.com>

	* dwarf2out.c (loc_descriptor): Pass entire MEM expressions
	through targetm.delegitimize_address.
	* config/arc/arc.c (arc_delegitimize_address): New function.
	(TARGET_DELEGITIMIZE_ADDRESS): Redefine.
	(arc_output_addr_const_extrao): Remove.
	* config/arc/arc.h (OUTPUT_ADDR_CONST_EXTRA): Remove.
	* config/arc/arc-protos.h (arc_output_addr_const_extra): Don't declare.

2008-07-23  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_output_libcall): Emit special sequence for
	-fpic -mlong-calls.
	* config/arc/arc.h (INSN_SETS_ARE_DELAYED): Also return true for
	TYPE_SFUNC.
	* config/arc/t-arc700-uClibc (SHLIB_MAPFILES): Define.
	* config/arc/arc.md (attribute type): Add sfunc.
	(attribute in_delay_slot): False for sfunc.
	(attribute in_sfunc_delay_slot): New.
	(toplevel): Add define_delay for sfunc.
	(cmpsf_eq, cmpdf_eq): Type sfunc for -fpic -mlong-call.
	(cmpsf_gt, cmpdf_gt, cmpsf_ge, cmpdf_ge: Likewise.
	(cmpsf_uneq, cmpdf_uneq, cmpsf_ord, cmpdf_ord): Likewise.
	* config/arc/lib1funcs.asm (HIDDEN_FUNC): Define.
	* config/arc/ieee-754/eqsf2.S, onfig/arc/ieee-754/orddf2.S: Use it.
	* config/arc/ieee-754/uneqsf2.S, config/arc/ieee-754/gtsf2.S: Likewise.
	* config/arc/ieee-754/gedf2.S, config/arc/ieee-754/ordsf2.S: Likewise.
	* config/arc/ieee-754/eqdf2.S, config/arc/ieee-754/uneqdf2.S: Likewise.
	* config/arc/ieee-754/gesf2.S, config/arc/ieee-754/gtdf2.S: Likewise.
	* config/arc/libgcc-excl.ver: New file.

2008-07-22  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_legitimize_pic_address): Use gen_const_mem.
	(arc_output_addr_const_extra): New function.
	* config/arc/arc-protos.h (arc_output_addr_const_extra): Declare.
	* config/arc/arc.h (LEGITIMIZE_ADDRESS): Don't pass OLDX.
	(OUTPUT_ADDR_CONST_EXTRA): Define.

2008-04-17  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (LINK_SPEC): Switch default back to -Bstatic.

2008-04-17  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_profile_call): Pass pointer as argument to
	_mcount_call.

2008-04-16  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (CONDITIONAL_REGISTER_USAGE): Adjust SIBCALL_REGS
	according to call_used_regs.
	* config/arc/gmon/mcount.c (_MCOUNT_DECL): Remove stray
	catomic_compare_and_exchange_bool_acq call.
	* testsuite/gcc.dg/func-ptr-prof.c: New file.

2008-04-09  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (LINK_SPEC) [USE_UCLIBC]: When profiling, use
	arclinux_prof emulation; else, use arclinux emulation.
	(LINK_SPEC) [!USE_UCLIBC]: When profiling, use arcelf_prof emulation.

2008-04-08  J"orn Rennecke  <joern.rennecke@arc.com>

	* config.gcc (arc600-*-linux-uclibc*): New configuration.
	* config/arc/t-arc600-uClibc: New file.

2008-04-05  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (gen_compare_reg): Swap operands for fpx
	CC_FP_GEmode comparisons.
	* config/arc/fpx.md (cmpsfpx_gt): Rename to:
	(cmpfpx_gt).  Also clear n flag if z flag is set.
	(cmpsfpx_ge): Rename to:
	(cmpfpx_ge).  Assume swapped comparison operands.  Don't
	setc flag if z flag is set.

2008-04-04  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/dp-hack.h (df_to_usi): Write as
	(tf_to_usi).

2008-04-04  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/fp-bit.c (_fpdiv_parts): Avoid double rounding.
	* config/arc/arc-protos.h (prepare_extend_operands): Move into part
	guarded by RTX_CODE.
	(arc_output_addsi, arc_expand_movmem, prepare_move_operands): Likewise.
	* config/arc/predicates.md (proper_comparison_operator):
	Fix CC_Cmode case.
	* config/arc/arc.c (branch_dest): Check for reversed branch.
	* config/arc/arc.h (LEGITIMATE_SCALED_ADDRESS_P): Add missing
	parentheses.
	* config/arc/arc.md (*movsi_scaled_insn): Remove pattern.
	(movdi_insn): Add %? output specifier.
	(movdf_insn): Likewise. Add cond attribute.

	* config/arc/arc-protos.h (gen_compare_reg): Update prototype.
	(arc_output_libcall): Declare.
	* config/arc/predicates.md (proper_comparison_operator):
	Add cases for floating point CCmodes, SFmode and DFmode.
	* config/arc/ieee-754/floatunsidf.S: New file.
	* config/arc/ieee-754/divdf3.S: Likewise.
	* config/arc/ieee-754/orddf2.S: Likewise.
	* config/arc/ieee-754/eqsf2.S: Likewise.
	* config/arc/ieee-754/truncdfsf2.S: Likewise.
	* config/arc/ieee-754/fixunsdfsi.S: Likewise.
	* config/arc/ieee-754/divtab-arc-df.c: Likewise.
	* config/arc/ieee-754/uneqsf2.S: Likewise.
	* config/arc/ieee-754/adddf3.S: Likewise.
	* config/arc/ieee-754/gtsf2.S: Likewise.
	* config/arc/ieee-754/gedf2.S: Likewise.
	* config/arc/ieee-754/floatsisf.S: Likewise.
	* config/arc/ieee-754/muldf3.S: Likewise.
	* config/arc/ieee-754/fixdfsi.S: Likewise.
	* config/arc/ieee-754/divsf3.S: Likewise.
	* config/arc/ieee-754/ordsf2.S: Likewise.
	* config/arc/ieee-754/eqdf2.S: Likewise.
	* config/arc/ieee-754/divtab-arc-sf.c: Likewise.
	* config/arc/ieee-754/divsf3-stdmul.S: Likewise.
	* config/arc/ieee-754/addsf3.S: Likewise.
	* config/arc/ieee-754/uneqdf2.S: Likewise.
	* config/arc/ieee-754/gesf2.S: Likewise.
	* config/arc/ieee-754/gtdf2.S: Likewise.
	* config/arc/ieee-754/mulsf3.S: Likewise.
	* config/arc/ieee-754/floatsidf.S: Likewise.
	* config/arc/ieee-754/fixsfsi.S: Likewise.
	* config/arc/ieee-754/arc-ieee-754.h: Likewise.
	* config/arc/ieee-754/extendsfdf2.S: Likewise.
	* config/arc/arc.c (get_arc_condition_code): Add floating point cases.
	(arc_select_cc_mode, arc_init_reg_tables): Likewise.
	(gen_compare_reg): Likewise.  Don't take comparison operands, but
	but result mode as parameter.  Return appropriate comparison of
	flags register against 0.  Changed all callers.
	(arc_output_libcall): New function.
	* config/arc/arc.h (INSN_SETS_ARE_DELAYED): Define.
	(INSN_REFERENCES_ARE_DELAYED, REVERSE_CONDITION): Likewise.
	* config/arc/fpx.md (cmpsfpx_raw, cmpdfpx_raw): New patterns.
	(cmpsfpx_gt, cmpsfpx_ge): Likewise.
	* config/arc/dp-hack.h: New file.
	* config/arc/fp-hack.h: New file.
	* config/arc/arc.md (movsicc_insn): Use proper_comparison_operator.
	(movdicc_insn, movsfcc_insn, movdfcc_insn, neg_scc_insn): Likewise.
	(not_scc_insn, movsi_cond_exec, add_cond_exec): Likewise.
	(commutative_cond_exec, sub_cond_exec): Likewise.
	(noncommutative_cond_exec): Likewise.
	(scc_insn): Use REVERSE_CONDITION.
	(bunge, bungt, bunle, bunlt, buneq, bltgt, bordered): New patterns.
	(bunordered, cmpsf, cmpdf, cmp_float, cmpsf_eq, cmpdf_eq): Likewise.
	(cmpsf_gt, cmpdf_gt, cmpsf_ge, cmpdf_ge, cmpsf_uneq: Likewise.
	(cmpdf_uneq, cmpsf_ord, cmpdf_ord, abssf2, negsf2): Likewise.
	* config/arc/lib1funcs.asm (FUNC, ENDFUNC0, ENDFUNC): New macros.
	(__udivsi3, __divsi3): Use FUNC / ENDFUNC.
	Toplevel: Add code to include files from ieee-754 subdirectory.
	* config/arc/t-arc (LIB1ASMFUNCS): Add floating-point functions.
	(dp-bit.c): cat config/arc/dp-hack.h.
	(fp-bit.c): cat config/arc/fp-hack.h.
	* config/arc/arc-modes.def (CC_FP_GT, CC_FP_GE, CC_FP_ORD): New modes.
	(CC_FP_UNEQ, CC_FPX): Likewise.

2008-03-27  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/t-arc700-uClibc (TARGET_LIBGCC2_CFLAGS): Add -fPIC.

2008-03-27  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (lr, sr): Enable r,r alternative.

2008-03-25  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (DRIVER_SELF_SPECS): Change condition to !mA*.

2008-03-20  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (DRIVER_SELF_SPECS): Change condition to !-mA.

2008-03-20  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (movqi_insn_mixed, loadqi_insn_mixed): Remove.
	(storeqi_insn_mixed, movhi_insn_mixed, loadhi_insn_mixed): Likewise.
	(storehi_insn_mixed, movsi_insn_mixed, loadsi_insn_mixed): Likewise.
	(storesi_insn_mixed): Likewise.
	(movhi_insn): Rewrite after model of movsi pattern.

2008-03-19  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_init): Fix default setting of processor
	type in target_flags.

2008-03-19  J"orn Rennecke  <joern.rennecke@arc.com>

	* reload.c (push_secondary_reload): Add missing break.
	* config/arc/arc.h (LEGITIMIZE_RELOAD_ADDRESS): Define.

2008-03-18  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (commutative_binary_comparison): Fix position
	of '%'.

2008-03-17  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (doloop_begin_i): Fix offset for loop end.

2008-03-06  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_encode_section_info): Use
	targetm.binds_local_p instead of TREE_PUBLIC.

2008-03-05  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/lib1funcs.asm (clzsi2) [!__ARC700__,!__A4__]:
	Re-implement to avoid undefined behaviour and increase performance.

	* config/arc/arc.h (CONDITIONAL_REGISTER_USAGE) <TARGET_ARC700>:
	Set fixed_regs[LP_COUNT].

2008-03-05  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (OVERRIDE_OPTIONS): Disable small data for -fPIC.
	(LEGITIMATE_SCALED_ADDRESS_P): For -fPIC, the only allowed constant
	bases are CONST_INT.

2008-02-27  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/constraints.md ("Cca", C2a): Check absolute value against
	maximum scaled constant.

2008-02-26  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/crti.asm, config/arc/crtn.asm: Fix stack layout.

2008-02-08  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/predicates.md (long_immediate_loadstore_operand):
	Also recognize that non-sdata SYMBOL_REFS require long immediates.

2008-02-07  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_print_operand_address): Always add @sda
	for small data symbols.  Use recursion.
	(prepare_move_operands): Also do small data address checks for
	floating point point modes.
	(prepare_extend_operands): New function.
	* config/arc/arc-protos.h (prepare_extend_operands): Declare.
	* config/arc/arc.md (zero_extendqihi2, zero_extendhisi2): Rename to:
	(*zero_extendqihi2_i, *zero_extendhisi2_i).
	(zero_extendqihi2, zero_extendhisi2): New expanders.
	(zero_extendqisi2, extendqisi2): Use prepare_extend_operands.
	(extendqihi2, extendhisi2): Rename to:
	(*extendqihi2_i, *extendhisi2_i).
	(extendqihi2i, extendhisi2): New expanders.

2008-02-07  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (doloop_begin_i): If loop is too large for lp
	instruction, emit code sequence using sr to set LOOP_START / LOOP_END
	appropriately.

2008-01-24  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (RTX_OK_FOR_OFFSET_P): Before applying scaling,
	check that offset is aligned.

2008-01-19  J"orn Rennecke  <joern.rennecke@arc.com>

	* resource.c (find_dead_or_set_registers): Don't consider sets inside
	a COND_EXEC to kill the value in a register.

2008-01-10  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/constraints.md ("Usc"): Reject small data.

	* config/arc/arc.md (movsi_insn): Fix predicate test for valid constant
	store address.

2008-01-08  J"orn Rennecke  <joern.rennecke@arc.com>

	* version.c (VERSUFFIX): Bump date.

2007-12-06  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc-protos.h (arc_asm_output_aligned_decl_common):
	Change to:
	(arc_asm_output_aligned_decl_local).
	* config/arc/arc.c: Likewise.
	* config/arc/arc.h (OPTIMIZATION_OPTIONS): Initialize flag_no_common
	as -1.
	(OVERRIDE_OPTIONS): If flag_no_common is still -1,  initialize according
	to TARGET_NO_SDATA_SET.
	(ASM_OUTPUT_ALIGNED_DECL_COMMON): Don't define.
	(ASM_OUTPUT_ALIGNED_DECL_LOCAL): Use arc_asm_output_aligned_decl_local.

2007-11-29  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (unspec_prof_hash): Only hash s1 string for
	SYMBOL_REF.
	(unspec_prof_htab_eq): Use rtx_equal_p.
	* config/arc/arc.md (call): Use emit_call_insn for profiling call.
	(call_value): Likewise.

2007-11-26  J"orn Rennecke  <joern.rennecke@arc.com>

	target/27758:
	* config/arc/arc.c (arc_output_pic_addr_const) <case SYMBOL_REF>:
	Use output_addr_const instead of assemble_name.

2007-11-20  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (movsi_insn): Add 'S' modifier for operand 1 of
	alternative 10.

2007-11-01  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc-protos.h (prepare_move_operands): Declare.
	* config/arc/predicates.md (move_dest_operand): Reject scaled index.
	(shouldbe_register_operand): New predicate.
	* config/arc/arc.c (arc_address_cost): Scaled index and plain index
	have low cost.
	(arc_print_operand): Print ".as" for scaled index.
	(arc_print_operand_address): Decompose scaled index.
	base can be CONST_INT.
	(arc_rewrite_small_data_1): If expression would get too complex,
	force small data address into register.
	(arc_rewrite_small_data): Pass &op as data.
	(prepare_move_operands): New function.
	* config/arc/arc.h (RTX_OK_FOR_INDEX_P): Enable.
	(LEGITIMATE_OFFSET_ADDRESS_P): Add INDEX parameter.  Changed all users.
	Require MODE size to be no larger than 4.
	(LEGITIMATE_SCALED_ADDRESS_P): Define.
	(GO_IF_LEGITIMATE_ADDRESS): Use LEGITIMATE_SCALED_ADDRESS_P.
	(GO_IF_MODE_DEPENDENT_ADDRESS): Only say REG-REG PRE/POST_MODIFY
	is mode dependent.
	Indexed addresses are mode dependent.
	* config/arc/arc.md (movqi, movhi, movsi, movdf): Use
	prepare_move_operands and prepare_move_operands.
	(movdi): Use move_dest_operand.
	(doloop_begin_i): Don't align if no lp instruction is used.
	(doloop_end_i): Now define_insn_and_split.
	* arc.opt (mindexed-load, mauto-modify-reg): New options.

2007-10-30  J"orn Rennecke  <joern.rennecke@arc.com>

	* Makefile.in (options.c): Depend on $(INSN_ATTR_H).  Include it.
	* config/arc/arc.h: Don't include insn-attr.h.

2007-10-26  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (doloop_fallback_m): Avoid unaligning code.

2007-10-26  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (doloop_begin_i): Add ????X alternative.
	(doloop_end_i): Add m alternative.
	(doloop_fallback_m): New pattern.
	* config/arc/arc.c (arc_reorg): Fix up doloop_end_i instructions
	that ended up with memory.  When a doloop_begin_i doesn't have the
	right register, try to find it in a preceding move.

2007-10-26  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_init): Provide default for arc_tune and
	arc_multcost.
	(arc_rtx_costs): Use arc_multcost.
	* arc.md: Move scheduling description to...
	* config/arc/arc700.md: ... here.  New file.
	* config/arc/arc600.md: New file.  Tie ARC700 scheduling description
	to tune_arc700 attribute.
	(mov_set_cc_insn, compare_insn): Combine to:
	(compare_700).
	(branch_700): New reservation.  Create new bypass.
	(multi_SI): Replace with:
	(multi_xmac, multi_std): New reservations.  Adjust bypasses.
	* arc.md: Update Copyright blurb.
	Include new files.
	(tune, tune_arc700): New attributes.
	(movsi_set_cc_insn): Set type to "compare".
	* config/arc/arc.h: Protect against multiple inclusion.
	Include "insn-attr.h".
	* arc.opt (multcost, mtune): New options.

2007-10-23  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (TRAMPOLINE_ADJUST_ADDRESS): Properly define.
	* config/arc/arc.md (sub_cond_exec): Add mode to set source.
	(doloop_begin_i): Add mode to operand 0.

	* doc/tm.texi (CAN_MOVE_BY_PIECES): Document.
	* expr.c (emit_block_move_via_movmem): No longer static.
	(can_move_by_pieces): New argument consider_movmem.
	Changed all callers.
	(expand_expr_real_1): Use can_move_by_pieces.
	* expr.h (emit_block_move_via_movmem): Declare.
	(can_move_by_pieces): Adjust declaration.
	* builtins.c (expand_builtin_mempcpy): Use target.
	Use emit_block_move_via_movmem.
	* config/arc/arc-protos.h (arc_expand_movmem): Declare.
	* config/arc/arc.c (force_offsettable): New function.
	(arc_expand_movmem): Likewise.
	* config/arc/arc.h (MOVE_MAX_PIECES): Don't define.
	(MOVE_BY_PIECES_P, CAN_MOVE_BY_PIECES, MOVE_RATIO): Define.
	* config/arc/arc.md (movmemsi): New pattern.

	* config/arc/divtab-arc700.c (main): Emit reverse ordered table.
	* config/arc/lib1funcs.asm (__divsi3): Use pcl as base addressing
	register with a negative index.

	* config/arc/predicates.md (cc_register): Now a special_predicate.
	(_2_4_8_operand): New predicate.
	* config/arc/arc.c (arc_rtx_costs): Add description for add_n and
	sub_n costs.
	* config/arc/arc.md (add_n, sub_n): New patterns.
	(shift_and_add_insn_mixed): Type shift.

	* combine.c (gen_lowpart_for_combine): Use omode when generating
	clobber.

2007-10-18  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc-protos.h (arc_save_restore): Don't declare.
	* config/arc/predicates.md (p2_immediate_operand): New predicate.
	(zn_compare_operator, commutative_operator): Likewise.
	(noncommutative_operator, unary_operator): Likewise.
	(move_dest_operand): Reject PRE_MODIFY / POST_MODIFY with
	register-register increment.
	(proper_comparison_operator): Replace.
	* config/arc/arc.c ("tm-constrs.h", "reload.h"): #include.
	(void arc_init): Don't set arc_punct_chars['~'].
	(get_arc_condition_code): Take mode into account.
	(arc_select_cc_mode): Replace.
	(arc_init_reg_tables): Update CC*mode handling.
	(arc_address_cost): Replace costs.
	(arc_save_restore): New arguments parity and first_offset.
	(arc_output_function_prologue): Emit short instructions.
	Fold stack adjustments into pre-modify addressing.
	(arc_output_function_epilogue): Likewise.
	Don't emit a nop when delay slot remains unfilled.
	If the delay insn is short, make jump long.
	(output_scaled): New variable.
	(arc_print_operand): Remove '~' case.
	Add support for scaled address offsets.
	Add 'B' and 'O' cases.
	(record_cc_ref): Remove COND_SET_ZNC handling.
	(arc_final_prescan_insn): Support -misize.
	(arc_reorg): Handle case when end of zero-overhead loop has been
	duplicated.
	When generating brcc insn, check that first comparison operand is
	a register.
	(arc_output_addsi): New function.
	* config/arc/arc.h (OPTIMIZATION_OPTIONS): Set default for
	arc_size_opt_level.
	(OVERRIDE_OPTIONS): When (arc_size_opt_level == 3), set optimize_size.
	(DATA_ALIGNMENT): Don't add extra alignment when arc_size_opt_level
	is 3.
	(SMALL_INT_RAGE): New macro.
	(HAVE_PRE_MODIFY_REG, HAVE_POST_MODIFY_REG): Define.
	(RTX_OK_FOR_OFFSET_P): Allow scaled indices.
	(LEGITIMATE_OFFSET_ADDRESS_P): Allow register-register
	PRE_MODIFY / POST_MODIFY.
	(JUMP_ALIGN, MOVE_MAX_PIECES): Define.
	(LABEL_ALIGN_AFTER_BARRIER, LOOP_ALIGN): Set to JUMP_ALIGN.
	* config/arc/arc.md (mov_set_cc_insn,core_insn, compare_insn):
	Align write_port allocation with other insn_reservation definitions.
	(atribute cond): Remove set_znc value.  Changed all users.
	(movqi_insn, movsi_insni, zero_extendqihi2): Rebalance alternatives.
	(zero_extendqisi2_aci, addsi3_mixed, subsi3_insn): Likewise.
	(cmpsi_cc_insn_mixed): Likewise.
	(movsi_set_cc_insn): Match both CC_ZNmode and CC_Zmode.
	(*movsi_insn_mixed): Changed another 'r' to 'c'.
	(unary_comparison, commutative_binary_comparison): New patterns.
	(noncommutative_binary_comparison, (cmpsi_cc_zn_insn): Likewise.
	(cmpsi_cc_z_insn, cmpsi_cc_c_insn, movsi_ne): Likewise.
	(movsi_cond_exec, add_cond_exec, commutative_cond_exec): Likewise.
	(sub_cond_exec, noncommutative_cond_exec): Likewise.
	(movdi_insn, movdf_insn): Add suport for scaled offsets.
	(zero_extendhisi2, extendhisi2): Likewise.
	(movsicc_insn, cbranchsi4_scratch): Tweak alternatives.
	(zero_extendhisi2): Add suport for scaled offsets.
	(addsi3_mixed): Use arc_output_addsi.
	(cmpsi_ccznc_insn_a4, cmpsi_cczn_insn_mixed): Deleted.
	(cmpsi_cczn_insn): Likewise.
	(scc_insn): Now a define_split.
	(branch_insn, rev_branch_insn, call_via_label): Don't output nops.
	(call_prof, call_value_via_label, call_value_prof): Likewise.
	(sibcall_value_insn, sibcall_prof, sibcall_value_prof): Lkewise.
	(flush_icache+3): Use CC_ZN_mode.
	(flush_icache+4): Likewise.  Check that operand[4] dies.
	(sibcall_insn): Add short alternative.
	(doloop_begin_i): Align loop insn.
	* config/arc/constraints.md (C_0, Usc, Rcq, Rs5): New constraints.
	(U): Rename to:
	(Usd).
	* config/arc/arc.opt (-malign-loops,-mno-align-loops): Remove.
	(msize-level=): Add.
	* config/arc/arc-modes.def (CCZNC, CCZN): Remove.
	(CC_ZN, CC_Z, CC_C): Add.

2007-10-15  J"orn Rennecke  <joern.rennecke@arc.com>

	* tree-ssa-loop-ch.c (copy_loop_headers): Suppress one case of
	counter-productive copying.
	* config/arc/arc-protos.h (arc_cond_exec_p): Declare.
	(arc_secondary_reload, arc_register_move_cost): Likewise.
	* config/arc/predicates.md (move_double_src_operand): Allow
	auto inc/dec.
	* config/arc/arc.c (arc_invalid_within_doloop): New function.
	(arc_cond_exec_p, arc_secondary_reload): Likewise.
	(arc_register_move_cost): Likewise.
	(TARGET_INVALID_WITHIN_DOLOOP): Define.
	(arc_print_operand): Handle POST_INC / POST_DEC / PRE_MODIFY /
	POST_MODIFY.
	(arc_reorg): Fix up zero-overhead loops.
	* config/arc/arc.h (FIRST_PSEUDO_REGISTER): Now 146.
	(FIXED_REGS): Make LP_COUNT non-fixed.
	(FIXED_REGS, CALL_USED_REGS, REG_CLASS_CONTENTS): Add LP_START & LP_END.
	Make LP_COUNT non-fixed.
	(PREFERRED_RELOAD_CLASS): Substitute GENERAL_REGS for CORE_REGS.
	(HAVE_POST_INCREMENT, HAVE_POST_DECREMENT): Define.
	(HAVE_PRE_MODIFY_DISP, HAVE_POST_MODIFY_DISP): Likewise.
	(SECONDARY_RELOAD_CLASS): Likewie.
	(GO_IF_LEGITIMATE_ADDRESS, GO_IF_MODE_DEPENDENT_ADDRESS): Add handling
	for POST_DEC / POST_INC / PRE_MODIFY / POST_MODIFY.
	(REGISTER_NAMES): Add lp_start and lp_end.
	* config/arc/arc.md (UNSPEC_LP, LP_COUNT, LP_START, LP_END): New
	constants.
	(type): Add loop.
	(in_delay_slot): No loops.
	(entire file): Use "c" constraint.
	(movdi_insn): Handle POST_INC / POST_DEC / PRE_MODIFY / POST_MODIFY.
	(movdf_insn): Likewise.
	(addsi3_mixed): Make conditional execution take precedence over
	short insns.  Improve instruction selection.
	(subsi3_insn): Add rsub support.
	(doloop_begin, doloop_begin_i, doloop_end, doloop_end_i): New patterns.
	(doloop_fallback): Likewise.
	* config/arc/constraints.md (Cca, C2a): New constraints.
	* config/arc/arc.opt (mexperimental-mask): New option.

2007-10-11  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/lib1funcs.asm (__udivsi3, __umodsi3): Comment out
	.__arc_profile_forward section data.

2007-10-08  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/t-arc-newlib (MULTILIB_OPTIONS): Remove mA4.
	(MULTILIB_DIRNAMES): Remove arc.
	* config/arc/arc.c (arc_init <!USE_UCLIB>): Default is A5.
	* config/arc/arc.h (CPP_SPEC): Default to __A5__.
	(ASM_SPEC): Default is -mA5.
	(MULTILIB_DEFAULTS): Set to { "mA5", "EL" } .

2007-10-03  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/gmon/gmon.c: #include <stddef.h> .

2007-10-01  J"orn Rennecke  <joern.rennecke@arc.com>

	* arc.h (LOCAL_ALIGNMENT): Define.

2007-10-01  J"orn Rennecke  <joern.rennecke@arc.com>

	* config.gcc (arc-*-elf*): Set tmake_file.
	(arc*-*-linux-uclibc*): Add arc/t-arc to tmake_file.
	* config/arc/t-arc: Use $@ / $<.
	(CRTSTUFF_T_CFLAGS_S): Remove.
	($(T)prof-freq-stub.o, $(T)dcache_linesz.o): New rules.
	($(T)profil-uclibc.o): Likewise.
	($(T)libgmon.a): Use $(PROFILE_OSDEP).
	(EXTRA_MULTILIB_PARTS): Remove mcount.o, gmon.o and prof-freq.o.
	(MULTILIB_OPTIONS, MULTILIB_DIRNAMES ULTILIB_MATCHES) Move to:
	* config/arc/t-arc-newlib: New file.
	(PPROFILE_OSDEP): Define.
	* config/arc/t-arc700-uClibc: Remove pieces redundant with t-arc.
	(PROFILE_OSDEP): Define.
	* config/arc/arc.h (STARTFILE_SPEC) <USE_UCLIBC>: Remove references
	to gcrt1.o.
	(ENDFILE_SPEC) <USE_UCLIBC> : Use crtgend.o for profiling.
	(LIB_SPEC) <USE_UCLIBC>: Use -lgmon -u profil --defsym __profil=profil
	for profiling.
	* config/arc/gmon/gmon.c (__profile_frequency): Remove.
	* config/arc/gmon/machine-gmon.h (_mcount_tailcall): Don't declare.
	* config/arc/gmon/profil.S (__profile_timer_cycles): New weak symbol.
	(__profil): Use it.  Fix FUNC usage.  Streamline ARC700 cache
	initialization a bit.
	(__dcache_linesz): Move to:
	* config/arc/gmon/dcache_linesz.S: New file.
	* config/arc/gmon/prof-freq-stub.S: New file.

2007-10-01  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/predicates.md (call_address_operand): Check CONSTANT_P
	before using LEGITIMATE_CONSTANT_P.

2007-09-28  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (DEFAULT_NO_SDATA): Define, set of USE_UCLIBC.
	(TARGET_DEFAULT_TARGET_FLAGS): Use it.

2007-09-19  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.md (sibcall_insn): Use call_address_operand.
	(sibcall_value_insn, sibcall_prof, sibcall_value_prof): Likewise.

2007-09-11  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c: Back out patches for callee-based profiling.
	* config/arc/arc.h: Likewise.

	* config/arc/arc-protos.h (arc_profile_call): Declare.
	* config/arc/arc.c (arc_initialize_trampoline): Generate oddly aligned
	trampoline.
	(write_profile_sections, unspec_prof_hash): New functions.
	(unspec_prof_htab_eq, arc_profile_call): Likewise.
	(arc_legitimate_constant_p): Allow UNSPEC_PROF.
	(arc_function_ok_for_sibcall): Allow long & indirect calls.
	Fix interrupt function check.
	(arc_reorg): Fix up UNSPEC_PROF and emit special profile sections.
	* config/arc/arc.h (ENDFILE_SPEC) <!USE_UCLIBC>: Add crtgend.
	(TRAMPOLINE_SIZE): Bump up to 20.
	(TRAMPOLINE_ADJUST_ADDRES): Define.
	* config/arc/gmon/gmon.c: Adjust for caller-based profiling.
	* config/arc/gmon/mcount.c, gmon/machine-gmon.h: Likewise.
	* config/arc/gmon/profil.S, config/arc/gmon/sys/gmon.h: Likewise.
	* config/arc/arc.md (UNSPEC_PROF): Define.
	(call, call_value, sibcall, sibcall_value): Add profiling code.
	(call_prof, call_value_prof, sibcall_prof): New patterns.
	(sibcall_value_prof): Likewise.
	(sibcall_insn, sibcall_value_insn): Enable long / indirect sibcalls.
	* config/arc/crtgend.asm: New file.
	* config/arc/lib1funcs.asm (__udivsi3, __umodsi3):
	Add .__arc_profile_forward entries.
	* config/arc/t-arc (crtgend.o) New target.
	(mcount.o): Remove -fcall-saved-r11.
	(gmon.o): Add -mno-sdata
	(EXTRA_MULTILIB_PARTS): Add crtgend.o
	* config/arc/constraints.md (Cbr): New constraint.
	* config/arc/crtg.asm: Add Start labels and alignment for
	profile sections.

	* config/arc/arc.h (enum reg_class): Add SIBCALL_REGS and CORE_REGS.
	(REG_CLASS_NAMES, REG_CLASS_CONTENTS): Likewise.
	* contraints.md (c): Change to:
	(Rgp).
	(c, Rsc): New constraints.
	* config/arc/arc.md (iorsi3): Use 'c' constraint.

2007-09-07  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (TARGET_VERSION): Fix spelling.
	* config/arc/t-arc700-uClibc (LIB1ASMFUNCS): Remove excess space.

	* config/arc/lib1funcs.asm (__modsi3): Make Ldivstart a local label.

	* config/arc/arc.h (STARTFILE_SPEC): Add -mkernel option.

	* config/arc/gmon/gmon.c (__profile_frequency): Define nonzero.
	(__monstartup): On error, clear p->tolimit.
	* config/arc/gmon/mcount.c (_MCOUNT_DECL): Don't use
	catomic_compare_and_exchange_bool_acq.
	* config/arc/gmon/machine-gmon.h: Don't use __builtin_frame_address.
	* config/arc/gmon/profil.S (__profil): Fix vector initialization.
	Enable interrrupts.
	(__profil_irq): Fix register assignment.  Make j.f explicit.

2007-08-29  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/t-arc700-uClibc (CRTSTUFF_T_CFLAGS): Add -mno-sdata .
	(MULTILIB_EXTRA_OPTS): Add mno-sdata .

2007-08-23  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (LINK_SPEC) [!USE_UCLIBC]: Use -marcelf.

	* config/arc/arc.h (STARTFILE_SPEC) [!USE_UCLIBC] [pg|p]: Add crtg.o.
	(LIB_SPEC) [!USE_UCLIBC]  [pg|p]: Add -lgmon.
	(REG_CLASS_CONTENTS): Add blink to GENERAL_REGS.
	(LARGE_INT): Fix cast type.
	(FUNCTION_PROFILER): Provide expansion.
	(PROFILE_BEFORE_PROLOGUE): Define.
	(NO_PROFILE_COUNTERS): Define.
	* config/arc/arc.c (MUST_SAVE_RETURN_ADDR): Take
	current_function_profile into account.
	(arc_compute_frame_size): Don't use MUST_SAVE_RETURN_ADDR as lvalue.
	arc_output_function_prologue): Don't push blink if it has already
	been pushed by the profiling code.
	* config/arc/gmon: New directory, containing atomic.h (empty),
	auxreg.h, gmon.c, machine-gmon.h, mcount.c, prof-freq.c, profil.S,
	sys/gmon.h, and sys/gmon_out.h .
	* config/arc/t-arc (crtg.o, mcount.o, gmon.o, prof-freq.o): New rules.
	(profil.o, libgmon.a): Likewise.
	(EXTRA_MULTILIB_PARTS): Add mcount.o, gmon.o, prof-freq.o, libgmon.a
	and crtg.o .
	* config/arc/asm.h, config/arc/crtg.asm: New files.
	

2007-05-29  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_decl_anon_ns_mem_p): New function, copied from
	cp/tree.c .
	(arc_in_small_data_p): Use default_binds_local_p_1 and
	arc_decl_anon_ns_mem_p to determine if a symbol binds locally.

2007-05-18  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/predicates.md (move_src_operand): Uncomment handling of
	CONST.

	* config/arc/arc.h (ASM_SPEC): Supply -mEA also for -mA7.

2007-05-14  J"orn Rennecke  <joern.rennecke@arc.com>

	* version.c: Add leading space.  Add suffix to denote ARC changes.

2007-05-14  J"orn Rennecke  <joern.rennecke@arc.com>

	Revert this patch:
	2007-04-19  J"orn Rennecke  <joern.rennecke@arc.com>
	  * output.h (get_unnamed_section_ggc): New macro.
	  * varasm.c (init_varasm_once): Use it.

	Instead, make section xmalloced:
	* c-pch.c (c_common_write_pch): Call pickle_in_section and
	unpickle_in_section.
	(c_common_read_pch): Call unpickle_in_section.
	* varasm.c (unnamed_sections): Remove GTY marker.
	(get_unnamed_section, get_noswitch_section): xmalloc section.
	(pickled_in_section): New static variable.
	(pickle_in_section, unpickle_in_section): New functions.
	* output.h (struct unnamed_section): Mark as GTY((skip)).
	(union section): Mark members unnamed_section and noswitch_section
	as GTY((skip)).
	(text_section, data_section, readonly_data_section): Remove GTY marker.
	(sdata_section, ctors_section, dtors_section, bss_section): Likewise.
	(sbss_section, tls_comm_section, comm_section): Likewise.
	(lcomm_section, bss_noswitch_section, in_section): Likewise.
	(pickle_in_section, unpickle_in_section): Declare.

	* config/arc/arc.c (arc_function_value): Mark unsignedp with
	ATTRIBUTE_UNUSED.

2007-04-26  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc-protos.h (arc_function_value): Declare.
	* config/arc/arc.h (FUNCTION_VALUE): Don't define.
	* config/arc/arc.c (TARGET_FUNCTION_VALUE): Define.
	(arc_function_value): New function.

2007-04-25  J"orn Rennecke  <joern.rennecke@arc.com>

	PR 20375
	* config/arc/arc.c (arc_setup_incoming_varargs): Don't abort on
	BLKmode.  Use arc_function_arg_advance.

2007-04-25  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (TARGET_MUST_PASS_IN_STACK):
	Set to must_pass_in_stack_var_size.

2007-04-25  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.h (TARGET_CPU_CPP_BUILTINS): Add __big_endian__ .
	(CPP_SPEC): Remove __big_endian__ .
	(ASM_SPEC, LINK_SPEC): For -mbig-endian, provide -EB.
	* config/arc/t-arc (MULTILIB_MATCHES): Add EB=mbig-endian .

2007-04-24  J"orn Rennecke  <joern.rennecke@arc.com>

	* config/arc/arc.c (arc_function_arg_advance): Only use
	ROUND_ADVANCE_CUM if the argument has a non-zero size.

2007-04-24  J"orn Rennecke  <joern.rennecke@arc.com>

	Fix profile based feedback:
	* config.gcc (arc-*-elf*): Don't use config/svr4.h.
	* config/arc/arc.h: Don't include config/svr4.h.

2007-04-23  J"orn Rennecke  <joern.rennecke@arc.com>

	From mainline:
	2006-11-11  Andrew Pinski  <andrew_pinski@playstation.sony.com>
	  PR rtl-opt/28812
	  * alias.c (fixed_scalar_and_varying_struct_p): Don't return a
	  non null value if the struct memory access is in the 0th
	  aliasing set.

2007-04-20  J"orn Rennecke  <joern.rennecke@arc.com>

	* longlong.h (__arc__): For __ARC700__, define umul_ppmm, UMUL_TIME,
	__umulsidi3, count_leading_zeros, COUNT_LEADING_ZEROS_0 .
	(COUNT_TRAILING_ZEROS_0): Define this according to
	COUNT_LEADING_ZEROES_0 when defining count_trailing_zeros using
	count_leading_zeroes.
	* libgcc2.c (__ffsSI2): Use COUNT_TRAILING_ZEROS_0.

	* config/arc/divtab-arc700.c: New file.

	Fix nested functions:
	* gcc/config/arc/arc.c (emit_store_direct): New function.
	(arc_initialize_trampoline): Initialize trampoline.
	* gcc/config/arc/arc.h (TRAMPOLINE_TEMPLATE): Disable definition
	which uses the wrong architecture and ABI.
	(TRAMPOLINE_ALIGNMENT): Define.
	(INITIALIZE_TRAMPOLINE): Use arc_initialize_trampoline.

	Fix stdarg argument passing:
	* gcc/config/arc/arc.c (arc_va_arg, arc_pass_by_reference): Pass
	variable size and addressable type by reference.
	(arc_arg_partial_bytes): Adjust CUM according to alignment of argument.
	(arc_function_arg, arc_function_arg_advance): Likewise.
	* gcc/config/arc/arc.h (ROUND_ADVANCE_CUM): Round.

	* gcc/config/arc/arc.h (TARGET_CPU_CPP_BUILTINS): Define
	__BIG_ENDIAN__ or __LITTLE_ENDIAN__, as the case might be.

	*  gcc/config/arc/arc.h (ASM_SPEC): For mARC700, pass -mEA.

2007-04-19  J"orn Rennecke  <joern.rennecke@arc.com>

	* output.h (get_unnamed_section_ggc): New macro.
	* varasm.c (init_varasm_once): Use it.
