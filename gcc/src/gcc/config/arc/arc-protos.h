/* Definitions of target machine for GNU compiler, Argonaut ARC cpu.
   Copyright (C) 2000 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef RTX_CODE
#ifdef TREE_CODE
extern void arc_va_start (tree, rtx);
extern rtx arc_va_arg (tree, tree);
#endif /* TREE_CODE */

extern enum machine_mode arc_select_cc_mode (enum rtx_code, rtx, rtx);

/* Define the function that build the compare insn for scc and bcc.  */
extern struct rtx_def *gen_compare_reg (enum rtx_code, enum machine_mode);

/* Declarations for various fns used in the .md file.  */
extern const char *output_shift (rtx *);
extern int compact_load_memory_operand (rtx op,enum machine_mode  mode);
extern int compact_sda_memory_operand (rtx op,enum machine_mode  mode);
extern int arc_valid_machine_decl_attribute (tree type,tree attributes,tree identifier,tree args);
extern int compact_store_memory_operand (rtx op,enum machine_mode  mode);
extern int u6_immediate_operand (rtx op,enum machine_mode mode);
extern int arc_double_register_operand (rtx op,enum machine_mode mode);
extern int symbolic_operand (rtx, enum machine_mode);
extern int arc_double_limm_p (rtx);
extern int arc_eligible_for_epilogue_delay (rtx, int);
extern void arc_initialize_trampoline (rtx, rtx, rtx);
extern void arc_print_operand (FILE *, rtx, int);
extern void arc_print_operand_address (FILE *, rtx);
extern void arc_final_prescan_insn (rtx, rtx *, int);
extern void arc_set_default_type_attributes(tree type);
extern int call_address_operand (rtx, enum machine_mode);
extern int call_operand (rtx, enum machine_mode);
extern int compact_register_operand (rtx op,enum machine_mode  mode);
extern int cc_register (rtx x, enum machine_mode mode);
extern int symbolic_memory_operand (rtx, enum machine_mode);
extern int short_immediate_operand (rtx, enum machine_mode);
extern int long_immediate_operand (rtx, enum machine_mode);
extern int long_immediate_loadstore_operand (rtx, enum machine_mode);
extern int move_src_operand (rtx, enum machine_mode);
extern int move_double_src_operand (rtx, enum machine_mode);
extern int move_dest_operand (rtx, enum machine_mode);
extern int load_update_operand (rtx, enum machine_mode);
extern int store_update_operand (rtx, enum machine_mode);
extern int nonvol_nonimm_operand (rtx, enum machine_mode);
extern int const_sint32_operand (rtx, enum machine_mode);
extern int const_uint32_operand (rtx, enum machine_mode);
extern int proper_comparison_operator (rtx, enum machine_mode);
extern int shift_operator (rtx, enum machine_mode);
extern int arc_dpfp_operator (rtx, enum machine_mode);
extern int arc_emit_vector_const (FILE *, rtx);
extern const char *arc_output_libcall (const char *);
extern int prepare_extend_operands (rtx *operands, enum rtx_code code,
				    enum machine_mode omode);
extern const char *arc_output_addsi (rtx *operands, const char *);
extern int arc_expand_movmem (rtx *operands);
extern int prepare_move_operands (rtx *operands, enum machine_mode mode);
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern enum arc_function_type arc_compute_function_type (tree);
extern void arc_setup_incoming_varargs (CUMULATIVE_ARGS *, enum machine_mode,
					tree, int *, int);
#endif /* TREE_CODE */


extern void arc_init (void);
extern unsigned int arc_compute_frame_size (int);
extern int arc_delay_slots_for_epilogue (void);
extern void arc_finalize_pic (void);
extern void arc_ccfsm_at_label (const char *, int);
extern int arc_ccfsm_branch_deleted_p (void);
extern void arc_ccfsm_record_branch_deleted (void);

extern rtx arc_legitimize_pic_address (rtx, rtx);
extern int arc_function_arg_partial_nregs (CUMULATIVE_ARGS *, enum machine_mode, tree,int);
extern rtx arc_function_arg (CUMULATIVE_ARGS *, enum machine_mode, tree, int);
extern void arc_function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode, tree, int);
extern rtx arc_function_value (tree, tree, bool);
void arc_asm_output_aligned_decl_local (FILE *, tree, const char *, 
					unsigned HOST_WIDE_INT, 
					unsigned HOST_WIDE_INT, 
					unsigned HOST_WIDE_INT);
extern rtx arc_return_addr_rtx (int , rtx );
extern int check_if_valid_regno_const (rtx *, int );
extern int check_if_valid_sleep_operand (rtx *, int );
extern int check_if_valid_sleep_operand (rtx *, int );
extern bool arc_legitimate_constant_p (rtx);
extern int arc_legitimate_pc_offset_p (rtx);
extern int arc_legitimate_pic_addr_p (rtx);
extern void arc_assemble_name (FILE *, const char*);
extern int symbolic_reference_mentioned_p (rtx);
extern void emit_pic_move (rtx *, enum machine_mode); 
extern int arc_raw_symbolic_reference_mentioned_p (rtx);
extern bool arc_legitimate_pic_operand_p (rtx);
extern const char * gen_bbit_insns(rtx *) ATTRIBUTE_UNUSED;
extern const char * gen_bbit_bic_insns(rtx *) ATTRIBUTE_UNUSED;
extern int valid_bbit_pattern_p (rtx *, rtx) ATTRIBUTE_UNUSED;
extern const char *arc_output_casesi_insn (rtx  *);
extern int arc_is_longcall_p (rtx, int);
extern int arc_profile_call (rtx callee);
extern int valid_brcc_with_delay_p (rtx *);
extern int small_data_pattern (rtx , enum machine_mode ATTRIBUTE_UNUSED);
extern rtx arc_rewrite_small_data (rtx);
extern int arc_cond_exec_p (void);
struct secondary_reload_info;
extern enum reg_class arc_secondary_reload (bool, rtx, enum reg_class,
					    enum machine_mode,
					    struct secondary_reload_info *);
extern int arc_register_move_cost (enum machine_mode, enum reg_class,
				   enum reg_class);
extern int arc_insn_length_adjustment (rtx);
extern int arc_corereg_hazard (rtx, rtx);
extern int arc_hazard (rtx, rtx);
extern int arc_write_ext_corereg (rtx);
