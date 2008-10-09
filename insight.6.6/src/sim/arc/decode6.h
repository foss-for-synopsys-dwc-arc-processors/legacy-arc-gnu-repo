/* Decode header for arc600f.

THIS FILE IS MACHINE GENERATED WITH CGEN.

Copyright 1996-2005 Free Software Foundation, Inc.

This file is part of the GNU simulators.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.

*/

#ifndef ARC600F_DECODE_H
#define ARC600F_DECODE_H

extern const IDESC *arc600f_decode (SIM_CPU *, IADDR,
                                  CGEN_INSN_INT, CGEN_INSN_INT,
                                  ARGBUF *);
extern void arc600f_init_idesc_table (SIM_CPU *);
extern void arc600f_sem_init_idesc_table (SIM_CPU *);
extern void arc600f_semf_init_idesc_table (SIM_CPU *);

/* Enum declaration for instructions in cpu family arc600f.  */
typedef enum arc600f_insn_type {
  ARC600F_INSN_X_INVALID, ARC600F_INSN_X_AFTER, ARC600F_INSN_X_BEFORE, ARC600F_INSN_X_CTI_CHAIN
 , ARC600F_INSN_X_CHAIN, ARC600F_INSN_X_BEGIN, ARC600F_INSN_B_S, ARC600F_INSN_BCC_S
 , ARC600F_INSN_BRCC_S, ARC600F_INSN_BCC_L, ARC600F_INSN_BCC_L_D, ARC600F_INSN_B_L
 , ARC600F_INSN_B_L_D, ARC600F_INSN_BRCC_RC, ARC600F_INSN_BRCC_RC_D, ARC600F_INSN_BRCC_U6
 , ARC600F_INSN_BRCC_U6_D, ARC600F_INSN_BL_S, ARC600F_INSN_BLCC, ARC600F_INSN_BLCC_D
 , ARC600F_INSN_BL, ARC600F_INSN_BL_D, ARC600F_INSN_LD_ABS, ARC600F_INSN_LD__AW_ABS
 , ARC600F_INSN_LD_AB_ABS, ARC600F_INSN_LD_AS_ABS, ARC600F_INSN_LD_ABC, ARC600F_INSN_LD__AW_ABC
 , ARC600F_INSN_LD_AB_ABC, ARC600F_INSN_LD_AS_ABC, ARC600F_INSN_LD_S_ABC, ARC600F_INSN_LD_S_ABU
 , ARC600F_INSN_LD_S_ABSP, ARC600F_INSN_LD_S_GPREL, ARC600F_INSN_LD_S_PCREL, ARC600F_INSN_LDB_ABS
 , ARC600F_INSN_LDB__AW_ABS, ARC600F_INSN_LDB_AB_ABS, ARC600F_INSN_LDB_AS_ABS, ARC600F_INSN_LDB_ABC
 , ARC600F_INSN_LDB__AW_ABC, ARC600F_INSN_LDB_AB_ABC, ARC600F_INSN_LDB_AS_ABC, ARC600F_INSN_LDB_S_ABC
 , ARC600F_INSN_LDB_S_ABU, ARC600F_INSN_LDB_S_ABSP, ARC600F_INSN_LDB_S_GPREL, ARC600F_INSN_LDB_X_ABS
 , ARC600F_INSN_LDB__AW_X_ABS, ARC600F_INSN_LDB_AB_X_ABS, ARC600F_INSN_LDB_AS_X_ABS, ARC600F_INSN_LDB_X_ABC
 , ARC600F_INSN_LDB__AW_X_ABC, ARC600F_INSN_LDB_AB_X_ABC, ARC600F_INSN_LDB_AS_X_ABC, ARC600F_INSN_LDW_ABS
 , ARC600F_INSN_LDW__AW_ABS, ARC600F_INSN_LDW_AB_ABS, ARC600F_INSN_LDW_AS_ABS, ARC600F_INSN_LDW_ABC
 , ARC600F_INSN_LDW__AW_ABC, ARC600F_INSN_LDW_AB_ABC, ARC600F_INSN_LDW_AS_ABC, ARC600F_INSN_LDW_S_ABC
 , ARC600F_INSN_LDW_S_ABU, ARC600F_INSN_LDW_S_GPREL, ARC600F_INSN_LDW_X_ABS, ARC600F_INSN_LDW__AW_X_ABS
 , ARC600F_INSN_LDW_AB_X_ABS, ARC600F_INSN_LDW_AS_X_ABS, ARC600F_INSN_LDW_X_ABC, ARC600F_INSN_LDW__AW_X_ABC
 , ARC600F_INSN_LDW_AB_X_ABC, ARC600F_INSN_LDW_AS_X_ABC, ARC600F_INSN_LDW_S_X_ABU, ARC600F_INSN_ST_ABS
 , ARC600F_INSN_ST__AW_ABS, ARC600F_INSN_ST_AB_ABS, ARC600F_INSN_ST_AS_ABS, ARC600F_INSN_ST_S_ABU
 , ARC600F_INSN_ST_S_ABSP, ARC600F_INSN_STB_ABS, ARC600F_INSN_STB__AW_ABS, ARC600F_INSN_STB_AB_ABS
 , ARC600F_INSN_STB_AS_ABS, ARC600F_INSN_STB_S_ABU, ARC600F_INSN_STB_S_ABSP, ARC600F_INSN_STW_ABS
 , ARC600F_INSN_STW__AW_ABS, ARC600F_INSN_STW_AB_ABS, ARC600F_INSN_STW_AS_ABS, ARC600F_INSN_STW_S_ABU
 , ARC600F_INSN_ADD_L_S12__RA_, ARC600F_INSN_ADD_CCU6__RA_, ARC600F_INSN_ADD_L_U6__RA_, ARC600F_INSN_ADD_L_R_R__RA__RC
 , ARC600F_INSN_ADD_CC__RA__RC, ARC600F_INSN_ADD_S_ABC, ARC600F_INSN_ADD_S_CBU3, ARC600F_INSN_ADD_S_MCAH
 , ARC600F_INSN_ADD_S_ABSP, ARC600F_INSN_ADD_S_ASSPSP, ARC600F_INSN_ADD_S_GP, ARC600F_INSN_ADD_S_R_U7
 , ARC600F_INSN_ADC_L_S12__RA_, ARC600F_INSN_ADC_CCU6__RA_, ARC600F_INSN_ADC_L_U6__RA_, ARC600F_INSN_ADC_L_R_R__RA__RC
 , ARC600F_INSN_ADC_CC__RA__RC, ARC600F_INSN_SUB_L_S12__RA_, ARC600F_INSN_SUB_CCU6__RA_, ARC600F_INSN_SUB_L_U6__RA_
 , ARC600F_INSN_SUB_L_R_R__RA__RC, ARC600F_INSN_SUB_CC__RA__RC, ARC600F_INSN_SUB_S_CBU3, ARC600F_INSN_I16_GO_SUB_S_GO
 , ARC600F_INSN_SUB_S_GO_SUB_NE, ARC600F_INSN_SUB_S_SSB, ARC600F_INSN_SUB_S_ASSPSP, ARC600F_INSN_SBC_L_S12__RA_
 , ARC600F_INSN_SBC_CCU6__RA_, ARC600F_INSN_SBC_L_U6__RA_, ARC600F_INSN_SBC_L_R_R__RA__RC, ARC600F_INSN_SBC_CC__RA__RC
 , ARC600F_INSN_AND_L_S12__RA_, ARC600F_INSN_AND_CCU6__RA_, ARC600F_INSN_AND_L_U6__RA_, ARC600F_INSN_AND_L_R_R__RA__RC
 , ARC600F_INSN_AND_CC__RA__RC, ARC600F_INSN_I16_GO_AND_S_GO, ARC600F_INSN_OR_L_S12__RA_, ARC600F_INSN_OR_CCU6__RA_
 , ARC600F_INSN_OR_L_U6__RA_, ARC600F_INSN_OR_L_R_R__RA__RC, ARC600F_INSN_OR_CC__RA__RC, ARC600F_INSN_I16_GO_OR_S_GO
 , ARC600F_INSN_BIC_L_S12__RA_, ARC600F_INSN_BIC_CCU6__RA_, ARC600F_INSN_BIC_L_U6__RA_, ARC600F_INSN_BIC_L_R_R__RA__RC
 , ARC600F_INSN_BIC_CC__RA__RC, ARC600F_INSN_I16_GO_BIC_S_GO, ARC600F_INSN_XOR_L_S12__RA_, ARC600F_INSN_XOR_CCU6__RA_
 , ARC600F_INSN_XOR_L_U6__RA_, ARC600F_INSN_XOR_L_R_R__RA__RC, ARC600F_INSN_XOR_CC__RA__RC, ARC600F_INSN_I16_GO_XOR_S_GO
 , ARC600F_INSN_MAX_L_S12__RA_, ARC600F_INSN_MAX_CCU6__RA_, ARC600F_INSN_MAX_L_U6__RA_, ARC600F_INSN_MAX_L_R_R__RA__RC
 , ARC600F_INSN_MAX_CC__RA__RC, ARC600F_INSN_MIN_L_S12__RA_, ARC600F_INSN_MIN_CCU6__RA_, ARC600F_INSN_MIN_L_U6__RA_
 , ARC600F_INSN_MIN_L_R_R__RA__RC, ARC600F_INSN_MIN_CC__RA__RC, ARC600F_INSN_MOV_L_S12_, ARC600F_INSN_MOV_CCU6_
 , ARC600F_INSN_MOV_L_U6_, ARC600F_INSN_MOV_L_R_R__RC, ARC600F_INSN_MOV_CC__RC, ARC600F_INSN_MOV_S_MCAH
 , ARC600F_INSN_MOV_S_MCAHB, ARC600F_INSN_MOV_S_R_U7, ARC600F_INSN_TST_L_S12_, ARC600F_INSN_TST_CCU6_
 , ARC600F_INSN_TST_L_U6_, ARC600F_INSN_TST_L_R_R__RC, ARC600F_INSN_TST_CC__RC, ARC600F_INSN_TST_S_GO
 , ARC600F_INSN_CMP_L_S12_, ARC600F_INSN_CMP_CCU6_, ARC600F_INSN_CMP_L_U6_, ARC600F_INSN_CMP_L_R_R__RC
 , ARC600F_INSN_CMP_CC__RC, ARC600F_INSN_CMP_S_MCAH, ARC600F_INSN_CMP_S_R_U7, ARC600F_INSN_RCMP_L_S12_
 , ARC600F_INSN_RCMP_CCU6_, ARC600F_INSN_RCMP_L_U6_, ARC600F_INSN_RCMP_L_R_R__RC, ARC600F_INSN_RCMP_CC__RC
 , ARC600F_INSN_RSUB_L_S12__RA_, ARC600F_INSN_RSUB_CCU6__RA_, ARC600F_INSN_RSUB_L_U6__RA_, ARC600F_INSN_RSUB_L_R_R__RA__RC
 , ARC600F_INSN_RSUB_CC__RA__RC, ARC600F_INSN_BSET_L_S12__RA_, ARC600F_INSN_BSET_CCU6__RA_, ARC600F_INSN_BSET_L_U6__RA_
 , ARC600F_INSN_BSET_L_R_R__RA__RC, ARC600F_INSN_BSET_CC__RA__RC, ARC600F_INSN_BSET_S_SSB, ARC600F_INSN_BCLR_L_S12__RA_
 , ARC600F_INSN_BCLR_CCU6__RA_, ARC600F_INSN_BCLR_L_U6__RA_, ARC600F_INSN_BCLR_L_R_R__RA__RC, ARC600F_INSN_BCLR_CC__RA__RC
 , ARC600F_INSN_BCLR_S_SSB, ARC600F_INSN_BTST_L_S12_, ARC600F_INSN_BTST_CCU6_, ARC600F_INSN_BTST_L_U6_
 , ARC600F_INSN_BTST_L_R_R__RC, ARC600F_INSN_BTST_CC__RC, ARC600F_INSN_BTST_S_SSB, ARC600F_INSN_BXOR_L_S12__RA_
 , ARC600F_INSN_BXOR_CCU6__RA_, ARC600F_INSN_BXOR_L_U6__RA_, ARC600F_INSN_BXOR_L_R_R__RA__RC, ARC600F_INSN_BXOR_CC__RA__RC
 , ARC600F_INSN_BMSK_L_S12__RA_, ARC600F_INSN_BMSK_CCU6__RA_, ARC600F_INSN_BMSK_L_U6__RA_, ARC600F_INSN_BMSK_L_R_R__RA__RC
 , ARC600F_INSN_BMSK_CC__RA__RC, ARC600F_INSN_BMSK_S_SSB, ARC600F_INSN_ADD1_L_S12__RA_, ARC600F_INSN_ADD1_CCU6__RA_
 , ARC600F_INSN_ADD1_L_U6__RA_, ARC600F_INSN_ADD1_L_R_R__RA__RC, ARC600F_INSN_ADD1_CC__RA__RC, ARC600F_INSN_I16_GO_ADD1_S_GO
 , ARC600F_INSN_ADD2_L_S12__RA_, ARC600F_INSN_ADD2_CCU6__RA_, ARC600F_INSN_ADD2_L_U6__RA_, ARC600F_INSN_ADD2_L_R_R__RA__RC
 , ARC600F_INSN_ADD2_CC__RA__RC, ARC600F_INSN_I16_GO_ADD2_S_GO, ARC600F_INSN_ADD3_L_S12__RA_, ARC600F_INSN_ADD3_CCU6__RA_
 , ARC600F_INSN_ADD3_L_U6__RA_, ARC600F_INSN_ADD3_L_R_R__RA__RC, ARC600F_INSN_ADD3_CC__RA__RC, ARC600F_INSN_I16_GO_ADD3_S_GO
 , ARC600F_INSN_SUB1_L_S12__RA_, ARC600F_INSN_SUB1_CCU6__RA_, ARC600F_INSN_SUB1_L_U6__RA_, ARC600F_INSN_SUB1_L_R_R__RA__RC
 , ARC600F_INSN_SUB1_CC__RA__RC, ARC600F_INSN_SUB2_L_S12__RA_, ARC600F_INSN_SUB2_CCU6__RA_, ARC600F_INSN_SUB2_L_U6__RA_
 , ARC600F_INSN_SUB2_L_R_R__RA__RC, ARC600F_INSN_SUB2_CC__RA__RC, ARC600F_INSN_SUB3_L_S12__RA_, ARC600F_INSN_SUB3_CCU6__RA_
 , ARC600F_INSN_SUB3_L_U6__RA_, ARC600F_INSN_SUB3_L_R_R__RA__RC, ARC600F_INSN_SUB3_CC__RA__RC, ARC600F_INSN_MPY_L_S12__RA_
 , ARC600F_INSN_MPY_CCU6__RA_, ARC600F_INSN_MPY_L_U6__RA_, ARC600F_INSN_MPY_L_R_R__RA__RC, ARC600F_INSN_MPY_CC__RA__RC
 , ARC600F_INSN_MPYH_L_S12__RA_, ARC600F_INSN_MPYH_CCU6__RA_, ARC600F_INSN_MPYH_L_U6__RA_, ARC600F_INSN_MPYH_L_R_R__RA__RC
 , ARC600F_INSN_MPYH_CC__RA__RC, ARC600F_INSN_MPYHU_L_S12__RA_, ARC600F_INSN_MPYHU_CCU6__RA_, ARC600F_INSN_MPYHU_L_U6__RA_
 , ARC600F_INSN_MPYHU_L_R_R__RA__RC, ARC600F_INSN_MPYHU_CC__RA__RC, ARC600F_INSN_MPYU_L_S12__RA_, ARC600F_INSN_MPYU_CCU6__RA_
 , ARC600F_INSN_MPYU_L_U6__RA_, ARC600F_INSN_MPYU_L_R_R__RA__RC, ARC600F_INSN_MPYU_CC__RA__RC, ARC600F_INSN_J_L_R_R___RC_NOILINK_
 , ARC600F_INSN_J_CC___RC_NOILINK_, ARC600F_INSN_J_L_R_R___RC_ILINK_, ARC600F_INSN_J_CC___RC_ILINK_, ARC600F_INSN_J_L_S12_
 , ARC600F_INSN_J_CCU6_, ARC600F_INSN_J_L_U6_, ARC600F_INSN_J_S, ARC600F_INSN_J_S__S
 , ARC600F_INSN_J_SEQ__S, ARC600F_INSN_J_SNE__S, ARC600F_INSN_J_L_S12_D_, ARC600F_INSN_J_CCU6_D_
 , ARC600F_INSN_J_L_U6_D_, ARC600F_INSN_J_L_R_R_D___RC_, ARC600F_INSN_J_CC_D___RC_, ARC600F_INSN_J_S_D
 , ARC600F_INSN_J_S__S_D, ARC600F_INSN_JL_L_S12_, ARC600F_INSN_JL_CCU6_, ARC600F_INSN_JL_L_U6_
 , ARC600F_INSN_JL_S, ARC600F_INSN_JL_L_R_R___RC_NOILINK_, ARC600F_INSN_JL_CC___RC_NOILINK_, ARC600F_INSN_JL_L_S12_D_
 , ARC600F_INSN_JL_CCU6_D_, ARC600F_INSN_JL_L_U6_D_, ARC600F_INSN_JL_L_R_R_D___RC_, ARC600F_INSN_JL_CC_D___RC_
 , ARC600F_INSN_JL_S_D, ARC600F_INSN_LP_L_S12_, ARC600F_INSN_LPCC_CCU6, ARC600F_INSN_FLAG_L_S12_
 , ARC600F_INSN_FLAG_CCU6_, ARC600F_INSN_FLAG_L_U6_, ARC600F_INSN_FLAG_L_R_R__RC, ARC600F_INSN_FLAG_CC__RC
 , ARC600F_INSN_LR_L_R_R___RC_, ARC600F_INSN_LR_L_S12_, ARC600F_INSN_SR_L_R_R___RC_, ARC600F_INSN_SR_L_S12_
 , ARC600F_INSN_ASL_L_R_R__RC, ARC600F_INSN_ASL_L_U6_, ARC600F_INSN_I16_GO_ASL_S_GO, ARC600F_INSN_ASR_L_R_R__RC
 , ARC600F_INSN_ASR_L_U6_, ARC600F_INSN_I16_GO_ASR_S_GO, ARC600F_INSN_LSR_L_R_R__RC, ARC600F_INSN_LSR_L_U6_
 , ARC600F_INSN_I16_GO_LSR_S_GO, ARC600F_INSN_ROR_L_R_R__RC, ARC600F_INSN_ROR_L_U6_, ARC600F_INSN_RRC_L_R_R__RC
 , ARC600F_INSN_RRC_L_U6_, ARC600F_INSN_SEXB_L_R_R__RC, ARC600F_INSN_SEXB_L_U6_, ARC600F_INSN_I16_GO_SEXB_S_GO
 , ARC600F_INSN_SEXW_L_R_R__RC, ARC600F_INSN_SEXW_L_U6_, ARC600F_INSN_I16_GO_SEXW_S_GO, ARC600F_INSN_EXTB_L_R_R__RC
 , ARC600F_INSN_EXTB_L_U6_, ARC600F_INSN_I16_GO_EXTB_S_GO, ARC600F_INSN_EXTW_L_R_R__RC, ARC600F_INSN_EXTW_L_U6_
 , ARC600F_INSN_I16_GO_EXTW_S_GO, ARC600F_INSN_ABS_L_R_R__RC, ARC600F_INSN_ABS_L_U6_, ARC600F_INSN_I16_GO_ABS_S_GO
 , ARC600F_INSN_NOT_L_R_R__RC, ARC600F_INSN_NOT_L_U6_, ARC600F_INSN_I16_GO_NOT_S_GO, ARC600F_INSN_RLC_L_R_R__RC
 , ARC600F_INSN_RLC_L_U6_, ARC600F_INSN_I16_GO_NEG_S_GO, ARC600F_INSN_SWI, ARC600F_INSN_TRAP_S
 , ARC600F_INSN_BRK, ARC600F_INSN_BRK_S, ARC600F_INSN_ASL_L_S12__RA_, ARC600F_INSN_ASL_CCU6__RA_
 , ARC600F_INSN_ASL_L_U6__RA_, ARC600F_INSN_ASL_L_R_R__RA__RC, ARC600F_INSN_ASL_CC__RA__RC, ARC600F_INSN_ASL_S_CBU3
 , ARC600F_INSN_ASL_S_SSB, ARC600F_INSN_I16_GO_ASLM_S_GO, ARC600F_INSN_LSR_L_S12__RA_, ARC600F_INSN_LSR_CCU6__RA_
 , ARC600F_INSN_LSR_L_U6__RA_, ARC600F_INSN_LSR_L_R_R__RA__RC, ARC600F_INSN_LSR_CC__RA__RC, ARC600F_INSN_LSR_S_SSB
 , ARC600F_INSN_I16_GO_LSRM_S_GO, ARC600F_INSN_ASR_L_S12__RA_, ARC600F_INSN_ASR_CCU6__RA_, ARC600F_INSN_ASR_L_U6__RA_
 , ARC600F_INSN_ASR_L_R_R__RA__RC, ARC600F_INSN_ASR_CC__RA__RC, ARC600F_INSN_ASR_S_CBU3, ARC600F_INSN_ASR_S_SSB
 , ARC600F_INSN_I16_GO_ASRM_S_GO, ARC600F_INSN_ROR_L_S12__RA_, ARC600F_INSN_ROR_CCU6__RA_, ARC600F_INSN_ROR_L_U6__RA_
 , ARC600F_INSN_ROR_L_R_R__RA__RC, ARC600F_INSN_ROR_CC__RA__RC, ARC600F_INSN_MUL64_L_S12_, ARC600F_INSN_MUL64_CCU6_
 , ARC600F_INSN_MUL64_L_U6_, ARC600F_INSN_MUL64_L_R_R__RC, ARC600F_INSN_MUL64_CC__RC, ARC600F_INSN_MUL64_S_GO
 , ARC600F_INSN_MULU64_L_S12_, ARC600F_INSN_MULU64_CCU6_, ARC600F_INSN_MULU64_L_U6_, ARC600F_INSN_MULU64_L_R_R__RC
 , ARC600F_INSN_MULU64_CC__RC, ARC600F_INSN_ADDS_L_S12__RA_, ARC600F_INSN_ADDS_CCU6__RA_, ARC600F_INSN_ADDS_L_U6__RA_
 , ARC600F_INSN_ADDS_L_R_R__RA__RC, ARC600F_INSN_ADDS_CC__RA__RC, ARC600F_INSN_SUBS_L_S12__RA_, ARC600F_INSN_SUBS_CCU6__RA_
 , ARC600F_INSN_SUBS_L_U6__RA_, ARC600F_INSN_SUBS_L_R_R__RA__RC, ARC600F_INSN_SUBS_CC__RA__RC, ARC600F_INSN_DIVAW_L_S12__RA_
 , ARC600F_INSN_DIVAW_CCU6__RA_, ARC600F_INSN_DIVAW_L_U6__RA_, ARC600F_INSN_DIVAW_L_R_R__RA__RC, ARC600F_INSN_DIVAW_CC__RA__RC
 , ARC600F_INSN_ASLS_L_S12__RA_, ARC600F_INSN_ASLS_CCU6__RA_, ARC600F_INSN_ASLS_L_U6__RA_, ARC600F_INSN_ASLS_L_R_R__RA__RC
 , ARC600F_INSN_ASLS_CC__RA__RC, ARC600F_INSN_ASRS_L_S12__RA_, ARC600F_INSN_ASRS_CCU6__RA_, ARC600F_INSN_ASRS_L_U6__RA_
 , ARC600F_INSN_ASRS_L_R_R__RA__RC, ARC600F_INSN_ASRS_CC__RA__RC, ARC600F_INSN_ADDSDW_L_S12__RA_, ARC600F_INSN_ADDSDW_CCU6__RA_
 , ARC600F_INSN_ADDSDW_L_U6__RA_, ARC600F_INSN_ADDSDW_L_R_R__RA__RC, ARC600F_INSN_ADDSDW_CC__RA__RC, ARC600F_INSN_SUBSDW_L_S12__RA_
 , ARC600F_INSN_SUBSDW_CCU6__RA_, ARC600F_INSN_SUBSDW_L_U6__RA_, ARC600F_INSN_SUBSDW_L_R_R__RA__RC, ARC600F_INSN_SUBSDW_CC__RA__RC
 , ARC600F_INSN_SWAP_L_R_R__RC, ARC600F_INSN_SWAP_L_U6_, ARC600F_INSN_NORM_L_R_R__RC, ARC600F_INSN_NORM_L_U6_
 , ARC600F_INSN_RND16_L_R_R__RC, ARC600F_INSN_RND16_L_U6_, ARC600F_INSN_ABSSW_L_R_R__RC, ARC600F_INSN_ABSSW_L_U6_
 , ARC600F_INSN_ABSS_L_R_R__RC, ARC600F_INSN_ABSS_L_U6_, ARC600F_INSN_NEGSW_L_R_R__RC, ARC600F_INSN_NEGSW_L_U6_
 , ARC600F_INSN_NEGS_L_R_R__RC, ARC600F_INSN_NEGS_L_U6_, ARC600F_INSN_NORMW_L_R_R__RC, ARC600F_INSN_NORMW_L_U6_
 , ARC600F_INSN_NOP_S, ARC600F_INSN_UNIMP_S, ARC600F_INSN_POP_S_B, ARC600F_INSN_POP_S_BLINK
 , ARC600F_INSN_PUSH_S_B, ARC600F_INSN_PUSH_S_BLINK, ARC600F_INSN_CURRENT_LOOP_END, ARC600F_INSN_CURRENT_LOOP_END_AFTER_BRANCH
 , ARC600F_INSN__MAX
} ARC600F_INSN_TYPE;

/* Enum declaration for semantic formats in cpu family arc600f.  */
typedef enum arc600f_sfmt_type {
  ARC600F_SFMT_EMPTY, ARC600F_SFMT_B_S, ARC600F_SFMT_BCC_S, ARC600F_SFMT_BRCC_S
 , ARC600F_SFMT_BCC_L, ARC600F_SFMT_B_L, ARC600F_SFMT_BRCC_RC, ARC600F_SFMT_BRCC_U6
 , ARC600F_SFMT_BL_S, ARC600F_SFMT_BLCC, ARC600F_SFMT_BL, ARC600F_SFMT_BL_D
 , ARC600F_SFMT_LD_ABS, ARC600F_SFMT_LD__AW_ABS, ARC600F_SFMT_LD_ABC, ARC600F_SFMT_LD__AW_ABC
 , ARC600F_SFMT_LD_S_ABC, ARC600F_SFMT_LD_S_ABU, ARC600F_SFMT_LD_S_ABSP, ARC600F_SFMT_LD_S_GPREL
 , ARC600F_SFMT_LD_S_PCREL, ARC600F_SFMT_LDB_ABS, ARC600F_SFMT_LDB__AW_ABS, ARC600F_SFMT_LDB_AS_ABS
 , ARC600F_SFMT_LDB_ABC, ARC600F_SFMT_LDB__AW_ABC, ARC600F_SFMT_LDB_AS_ABC, ARC600F_SFMT_LDB_S_ABC
 , ARC600F_SFMT_LDB_S_ABU, ARC600F_SFMT_LDB_S_ABSP, ARC600F_SFMT_LDB_S_GPREL, ARC600F_SFMT_LDW_ABS
 , ARC600F_SFMT_LDW__AW_ABS, ARC600F_SFMT_LDW_ABC, ARC600F_SFMT_LDW__AW_ABC, ARC600F_SFMT_LDW_S_ABC
 , ARC600F_SFMT_LDW_S_ABU, ARC600F_SFMT_LDW_S_GPREL, ARC600F_SFMT_ST_ABS, ARC600F_SFMT_ST__AW_ABS
 , ARC600F_SFMT_ST_S_ABU, ARC600F_SFMT_ST_S_ABSP, ARC600F_SFMT_STB_ABS, ARC600F_SFMT_STB__AW_ABS
 , ARC600F_SFMT_STB_AS_ABS, ARC600F_SFMT_STB_S_ABU, ARC600F_SFMT_STB_S_ABSP, ARC600F_SFMT_STW_ABS
 , ARC600F_SFMT_STW__AW_ABS, ARC600F_SFMT_STW_S_ABU, ARC600F_SFMT_ADD_L_S12__RA_, ARC600F_SFMT_ADD_CCU6__RA_
 , ARC600F_SFMT_ADD_L_U6__RA_, ARC600F_SFMT_ADD_L_R_R__RA__RC, ARC600F_SFMT_ADD_CC__RA__RC, ARC600F_SFMT_ADD_S_ABC
 , ARC600F_SFMT_ADD_S_CBU3, ARC600F_SFMT_ADD_S_MCAH, ARC600F_SFMT_ADD_S_ABSP, ARC600F_SFMT_ADD_S_ASSPSP
 , ARC600F_SFMT_ADD_S_GP, ARC600F_SFMT_ADD_S_R_U7, ARC600F_SFMT_ADC_L_S12__RA_, ARC600F_SFMT_ADC_CCU6__RA_
 , ARC600F_SFMT_ADC_L_U6__RA_, ARC600F_SFMT_ADC_L_R_R__RA__RC, ARC600F_SFMT_ADC_CC__RA__RC, ARC600F_SFMT_I16_GO_SUB_S_GO
 , ARC600F_SFMT_SUB_S_GO_SUB_NE, ARC600F_SFMT_SUB_S_SSB, ARC600F_SFMT_AND_L_S12__RA_, ARC600F_SFMT_AND_CCU6__RA_
 , ARC600F_SFMT_AND_L_U6__RA_, ARC600F_SFMT_AND_L_R_R__RA__RC, ARC600F_SFMT_AND_CC__RA__RC, ARC600F_SFMT_MOV_L_S12_
 , ARC600F_SFMT_MOV_CCU6_, ARC600F_SFMT_MOV_L_U6_, ARC600F_SFMT_MOV_L_R_R__RC, ARC600F_SFMT_MOV_CC__RC
 , ARC600F_SFMT_MOV_S_MCAH, ARC600F_SFMT_MOV_S_MCAHB, ARC600F_SFMT_MOV_S_R_U7, ARC600F_SFMT_TST_L_S12_
 , ARC600F_SFMT_TST_CCU6_, ARC600F_SFMT_TST_L_U6_, ARC600F_SFMT_TST_L_R_R__RC, ARC600F_SFMT_TST_CC__RC
 , ARC600F_SFMT_TST_S_GO, ARC600F_SFMT_CMP_L_S12_, ARC600F_SFMT_CMP_CCU6_, ARC600F_SFMT_CMP_L_U6_
 , ARC600F_SFMT_CMP_L_R_R__RC, ARC600F_SFMT_CMP_CC__RC, ARC600F_SFMT_CMP_S_MCAH, ARC600F_SFMT_CMP_S_R_U7
 , ARC600F_SFMT_BTST_S_SSB, ARC600F_SFMT_MPY_L_S12__RA_, ARC600F_SFMT_MPY_CCU6__RA_, ARC600F_SFMT_MPY_L_U6__RA_
 , ARC600F_SFMT_MPY_L_R_R__RA__RC, ARC600F_SFMT_MPY_CC__RA__RC, ARC600F_SFMT_J_L_R_R___RC_NOILINK_, ARC600F_SFMT_J_CC___RC_NOILINK_
 , ARC600F_SFMT_J_L_R_R___RC_ILINK_, ARC600F_SFMT_J_CC___RC_ILINK_, ARC600F_SFMT_J_L_S12_, ARC600F_SFMT_J_CCU6_
 , ARC600F_SFMT_J_L_U6_, ARC600F_SFMT_J_S, ARC600F_SFMT_J_S__S, ARC600F_SFMT_J_L_S12_D_
 , ARC600F_SFMT_J_CCU6_D_, ARC600F_SFMT_J_L_U6_D_, ARC600F_SFMT_J_L_R_R_D___RC_, ARC600F_SFMT_J_CC_D___RC_
 , ARC600F_SFMT_JL_L_S12_, ARC600F_SFMT_JL_CCU6_, ARC600F_SFMT_JL_L_U6_, ARC600F_SFMT_JL_S
 , ARC600F_SFMT_JL_L_R_R___RC_NOILINK_, ARC600F_SFMT_JL_CC___RC_NOILINK_, ARC600F_SFMT_JL_L_R_R_D___RC_, ARC600F_SFMT_JL_CC_D___RC_
 , ARC600F_SFMT_JL_S_D, ARC600F_SFMT_LP_L_S12_, ARC600F_SFMT_LPCC_CCU6, ARC600F_SFMT_FLAG_L_S12_
 , ARC600F_SFMT_FLAG_CCU6_, ARC600F_SFMT_FLAG_L_U6_, ARC600F_SFMT_FLAG_L_R_R__RC, ARC600F_SFMT_FLAG_CC__RC
 , ARC600F_SFMT_LR_L_R_R___RC_, ARC600F_SFMT_LR_L_S12_, ARC600F_SFMT_SR_L_R_R___RC_, ARC600F_SFMT_SR_L_S12_
 , ARC600F_SFMT_ASL_L_R_R__RC, ARC600F_SFMT_ASL_L_U6_, ARC600F_SFMT_ASR_L_R_R__RC, ARC600F_SFMT_ASR_L_U6_
 , ARC600F_SFMT_RRC_L_R_R__RC, ARC600F_SFMT_RRC_L_U6_, ARC600F_SFMT_SEXB_L_R_R__RC, ARC600F_SFMT_SEXB_L_U6_
 , ARC600F_SFMT_SEXW_L_R_R__RC, ARC600F_SFMT_SEXW_L_U6_, ARC600F_SFMT_ABS_L_R_R__RC, ARC600F_SFMT_ABS_L_U6_
 , ARC600F_SFMT_NOT_L_R_R__RC, ARC600F_SFMT_NOT_L_U6_, ARC600F_SFMT_SWI, ARC600F_SFMT_TRAP_S
 , ARC600F_SFMT_BRK, ARC600F_SFMT_ASL_L_S12__RA_, ARC600F_SFMT_ASL_CCU6__RA_, ARC600F_SFMT_ASL_L_U6__RA_
 , ARC600F_SFMT_ASL_L_R_R__RA__RC, ARC600F_SFMT_ASL_CC__RA__RC, ARC600F_SFMT_MUL64_L_S12_, ARC600F_SFMT_MUL64_CCU6_
 , ARC600F_SFMT_MUL64_L_U6_, ARC600F_SFMT_MUL64_L_R_R__RC, ARC600F_SFMT_MUL64_CC__RC, ARC600F_SFMT_MUL64_S_GO
 , ARC600F_SFMT_ADDS_L_S12__RA_, ARC600F_SFMT_ADDS_CCU6__RA_, ARC600F_SFMT_ADDS_L_U6__RA_, ARC600F_SFMT_ADDS_L_R_R__RA__RC
 , ARC600F_SFMT_ADDS_CC__RA__RC, ARC600F_SFMT_DIVAW_L_S12__RA_, ARC600F_SFMT_DIVAW_CCU6__RA_, ARC600F_SFMT_DIVAW_L_U6__RA_
 , ARC600F_SFMT_DIVAW_L_R_R__RA__RC, ARC600F_SFMT_DIVAW_CC__RA__RC, ARC600F_SFMT_ASLS_L_S12__RA_, ARC600F_SFMT_ASLS_CCU6__RA_
 , ARC600F_SFMT_ASLS_L_U6__RA_, ARC600F_SFMT_ASLS_L_R_R__RA__RC, ARC600F_SFMT_ASLS_CC__RA__RC, ARC600F_SFMT_SWAP_L_R_R__RC
 , ARC600F_SFMT_SWAP_L_U6_, ARC600F_SFMT_NORM_L_U6_, ARC600F_SFMT_RND16_L_R_R__RC, ARC600F_SFMT_RND16_L_U6_
 , ARC600F_SFMT_ABSSW_L_R_R__RC, ARC600F_SFMT_ABSSW_L_U6_, ARC600F_SFMT_ABSS_L_U6_, ARC600F_SFMT_NOP_S
 , ARC600F_SFMT_POP_S_B, ARC600F_SFMT_POP_S_BLINK, ARC600F_SFMT_PUSH_S_B, ARC600F_SFMT_PUSH_S_BLINK
 , ARC600F_SFMT_CURRENT_LOOP_END, ARC600F_SFMT_CURRENT_LOOP_END_AFTER_BRANCH
} ARC600F_SFMT_TYPE;

/* Function unit handlers (user written).  */

extern int arc600f_model_ARC600_u_exec (SIM_CPU *, const IDESC *, int /*unit_num*/, int /*referenced*/, INT /*b*/, INT /*c*/, INT /*a*/);

/* Profiling before/after handlers (user written) */

extern void arc600f_model_insn_before (SIM_CPU *, int /*first_p*/);
extern void arc600f_model_insn_after (SIM_CPU *, int /*last_p*/, int /*cycles*/);

#endif /* ARC600F_DECODE_H */