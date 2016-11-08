(define_predicate "dest_reg_operand"
  (match_code "reg,subreg")
{
  rtx op0 = op;

  if (GET_CODE (op0) == SUBREG)
    op0 = SUBREG_REG (op0);
  if (REG_P (op0) && REGNO (op0) < FIRST_PSEUDO_REGISTER
      && TEST_HARD_REG_BIT (reg_class_contents[ALL_CORE_REGS],
			    REGNO (op0))
      && !TEST_HARD_REG_BIT (reg_class_contents[WRITABLE_CORE_REGS],
			    REGNO (op0)))
    return 0;
  return register_operand (op, mode);
})

(define_predicate "mpy_dest_reg_operand"
  (match_code "reg,subreg")
{
  rtx op0 = op;

  if (GET_CODE (op0) == SUBREG)
    op0 = SUBREG_REG (op0);
  if (REG_P (op0) && REGNO (op0) < FIRST_PSEUDO_REGISTER
      && TEST_HARD_REG_BIT (reg_class_contents[ALL_CORE_REGS],
			    REGNO (op0))
      /* Make sure the destination register is not LP_COUNT.  */
      && !TEST_HARD_REG_BIT (reg_class_contents[MPY_WRITABLE_CORE_REGS],
			    REGNO (op0)))
    return 0;
  return register_operand (op, mode);
})


;; Returns 1 if OP is a symbol reference.
(define_predicate "symbolic_operand"
  (match_code "symbol_ref, label_ref, const")
)

;; Acceptable arguments to the call insn.
(define_predicate "call_address_operand"
  (ior (match_code "const_int, reg")
       (match_operand 0 "symbolic_operand")
       (match_test "CONSTANT_P (op) && LEGITIMATE_CONSTANT_P (op)"))
)

(define_predicate "call_operand"
  (and (match_code "mem")
       (match_test "call_address_operand (XEXP (op, 0), mode)"))
)

;; Return truth value of statement that OP is a symbolic memory
;; operand of mode MODE.
(define_predicate "symbolic_memory_operand"
  (match_code "subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST
	  || GET_CODE (op) == LABEL_REF);
}  
)

;; Return true if OP is a unsigned 6-bit immediate (u6) value.
(define_predicate "u6_immediate_operand"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT6 (INTVAL (op))"))
)

;; Return true if OP is a short immediate (shimm) value.  
(define_predicate "short_immediate_operand"
  (and (match_code "const_int")
       (match_test "SMALL_INT (INTVAL (op))"))
)

(define_predicate "p2_immediate_operand"
  (and (match_code "const_int")
       (match_test "((INTVAL (op) - 1) & INTVAL (op)) == 0")
       (match_test "INTVAL (op)"))
)

;; Return true if OP will require a long immediate (limm) value.
;; This is currently only used when calculating length attributes.
(define_predicate "long_immediate_operand"
  (match_code "symbol_ref, label_ref, const, const_double, const_int")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
    case LABEL_REF :
    case CONST :
      return 1;
    case CONST_INT :
      return (TARGET_A4) ? !SMALL_INT (INTVAL (op))
                         : !SIGNED_INT12 (INTVAL (op));
    case CONST_DOUBLE :
      /* These can happen because large unsigned 32 bit constants are
	 represented this way (the multiplication patterns can cause these
	 to be generated).  They also occur for SFmode values.  */
      return 1;
    default:
      break;
    }
  return 0;
}  
)

;; Return true if OP is a MEM that when used as a load or store address will
;; require an 8 byte insn.
;; Load and store instructions don't allow the same possibilities but they're
;; similar enough that this one function will do.
;; This is currently only used when calculating length attributes.  */
(define_predicate "long_immediate_loadstore_operand"
  (match_code "mem")
{
  op = XEXP (op, 0);
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
    case LABEL_REF :
    case CONST :
      return 1;
    case CONST_INT :
      /* This must be handled as "st c,[limm]".  Ditto for load.
	 Technically, the assembler could translate some possibilities to
	 "st c,[limm/2 + limm/2]" if limm/2 will fit in a shimm, but we don't
	 assume that it does.  */
      return 1;
    case CONST_DOUBLE :
      /* These can happen because large unsigned 32 bit constants are
	 represented this way (the multiplication patterns can cause these
	 to be generated).  They also occur for SFmode values.  */
      return 1;
    case REG :
      return 0;
    case PLUS :
      {
	rtx x = XEXP (op, 1);

	if (GET_CODE (x) == CONST)
	  {
	    x = XEXP (x, 0);
	    if (GET_CODE (x) == PLUS)
	      x = XEXP (x, 0);
	  }
	if (CONST_INT_P (x))
	  return !SMALL_INT (INTVAL (x));
	else if (GET_CODE (x) == SYMBOL_REF)
	  return TARGET_NO_SDATA_SET || !SYMBOL_REF_SMALL_P (x);
	return 0;
      }
    default:
      break;
    }
  return 0;
}
)

;; Return true if OP is any of R0-R3,R12-R15 for ARCompact 16-bit 
;; instructions
(define_predicate "compact_register_operand"
  (match_code "reg, subreg")
  {
     if ((GET_MODE (op) != mode) && (mode != VOIDmode))
         return 0;

      return (GET_CODE (op) == REG)
      && (REGNO (op) >= FIRST_PSEUDO_REGISTER
		|| COMPACT_GP_REG_P (REGNO (op))) ;
  }
)

;; Return true if OP is an acceptable memory operand for ARCompact
;; 16-bit load instructions.
(define_predicate "compact_load_memory_operand"
  (match_code "mem")
{
  rtx addr, plus0, plus1;
  int size, off;

  /* Eliminate non-memory operations */
  if (GET_CODE (op) != MEM)
    return 0;

  /* .di instructions have no 16-bit form */
  if (!TARGET_VOLATILE_CACHE_SET)
     return 0;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  size = GET_MODE_SIZE (mode);

  /* dword operations really put out 2 instructions, so eliminate them. */ 
  if (size > UNITS_PER_WORD)
    return 0;

  /* Decode the address now.  */
  addr = XEXP (op, 0);
  switch (GET_CODE (addr))
    {
    case REG:
      return (REGNO (addr) >= FIRST_PSEUDO_REGISTER
                || COMPACT_GP_REG_P (REGNO (addr))
	      || (SP_REG_P (REGNO (addr)) && (size != 2)));
	/* Reverting for the moment since ldw_s  does not have sp as a valid
	   parameter */
    case PLUS:
      plus0 = XEXP (addr, 0);
      plus1 = XEXP (addr, 1);
 
      if ((GET_CODE (plus0) == REG)
          && ((REGNO (plus0) >= FIRST_PSEUDO_REGISTER)
              || COMPACT_GP_REG_P (REGNO (plus0)))
          && ((GET_CODE (plus1) == REG)
              && ((REGNO (plus1) >= FIRST_PSEUDO_REGISTER)
                  || COMPACT_GP_REG_P (REGNO (plus1)))))
        {
          return 1;
        }

      if ((GET_CODE (plus0) == REG)
          && ((REGNO (plus0) >= FIRST_PSEUDO_REGISTER)
              || COMPACT_GP_REG_P (REGNO (plus0)))
          && (GET_CODE (plus1) == CONST_INT))
        {
          off = INTVAL (plus1);

          /* negative offset is not supported in 16-bit load/store insns. */
          if (off < 0)
            return 0;

          switch (size)
            {
            case 1:
              return (off < 32);
            case 2:
              return ((off < 64) && (off % 2 == 0));
            case 4:
              return ((off < 128) && (off % 4 == 0));
            }
        }

      if ((GET_CODE (plus0) == REG)
          && ((REGNO (plus0) >= FIRST_PSEUDO_REGISTER)
              || SP_REG_P (REGNO (plus0)))
          && (GET_CODE (plus1) == CONST_INT))
        {
          off = INTVAL (plus1);
          return ((size != 2) && (off >= 0 && off < 128) && (off % 4 == 0));
        }
    default:
      break ;
      /* TODO: 'gp' and 'pcl' are to supported as base address operand
               for 16-bit load instructions. */
    }
  return 0;

}
)

;; Return true if OP is an acceptable memory operand for ARCompact
;; 16-bit store instructions
(define_predicate "compact_store_memory_operand"
  (match_code "mem")
{
  rtx addr, plus0, plus1;
  int size, off;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  /* .di instructions have no 16-bit form */
  if (!TARGET_VOLATILE_CACHE_SET)
     return 0;

  size = GET_MODE_SIZE (mode);

  /* dword operations really put out 2 instructions, so eliminate them. */ 
  if (size > UNITS_PER_WORD)
    return 0;

  /* Decode the address now.  */
  addr = XEXP (op, 0);
  switch (GET_CODE (addr))
    {
    case REG:
      return (REGNO (addr) >= FIRST_PSEUDO_REGISTER
                || COMPACT_GP_REG_P (REGNO (addr))
	      || (SP_REG_P (REGNO (addr)) && (size != 2)));
	/* stw_s does not support SP as a parameter */
    case PLUS:
      plus0 = XEXP (addr, 0);
      plus1 = XEXP (addr, 1);
 
      if ((GET_CODE (plus0) == REG)
          && ((REGNO (plus0) >= FIRST_PSEUDO_REGISTER)
              || COMPACT_GP_REG_P (REGNO (plus0)))
          && (GET_CODE (plus1) == CONST_INT))
        {
          off = INTVAL (plus1);

          /* negative offset is not supported in 16-bit load/store insns. */
          if (off < 0)
            return 0;

          switch (size)
            {
            case 1:
              return (off < 32);
            case 2:
              return ((off < 64) && (off % 2 == 0));
            case 4:
              return ((off < 128) && (off % 4 == 0));
            }
        }

      if ((GET_CODE (plus0) == REG)
          && ((REGNO (plus0) >= FIRST_PSEUDO_REGISTER)
              || SP_REG_P (REGNO (plus0)))
          && (GET_CODE (plus1) == CONST_INT))
        {
          off = INTVAL (plus1);

          return ((size != 2) && (off >= 0 && off < 128) && (off % 4 == 0));
        }
    default:
      break;
    }
  return 0;
  }
)

;; Return true if OP is an acceptable argument for a single word
;;   move source.
(define_predicate "move_src_operand"
  (match_code "symbol_ref, label_ref, const, const_int, const_double, reg, subreg, mem")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
    case LABEL_REF :
    case CONST :
/*ashwin : The use of this macro is discontinued       */
/*     case CONSTANT_P_RTX: */
      return (!flag_pic || arc_legitimate_pic_operand_p(op));
    case CONST_INT :
      return (LARGE_INT (INTVAL (op)));
    case CONST_DOUBLE :
      /* We can handle DImode integer constants in SImode if the value
	 (signed or unsigned) will fit in 32 bits.  This is needed because
	 large unsigned 32 bit constants are represented as CONST_DOUBLEs.  */
      if (mode == SImode)
	return arc_double_limm_p (op);
      /* We can handle 32 bit floating point constants.  */
      if (mode == SFmode)
	return GET_MODE (op) == SFmode;
      return 0;
    case REG :
      return register_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	return address_operand (XEXP (SUBREG_REG (op), 0), mode);
      else
	return register_operand (op, mode);
    case MEM :
      return address_operand (XEXP (op, 0), mode);
    default :
      return 0;
    }
}
)

;; Return true if OP is an acceptable argument for a double word
;; move source.
(define_predicate "move_double_src_operand"
  (match_code "reg, subreg, mem, const_int, const_double")
{
  switch (GET_CODE (op))
    {
    case REG :
      return register_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	return move_double_src_operand (SUBREG_REG (op), mode);
      else
	return register_operand (op, mode);
    case MEM :
      return address_operand (XEXP (op, 0), mode);
    case CONST_INT :
    case CONST_DOUBLE :
      return 1;
    default :
      return 0;
    }
}
)

;; Return true if OP is an acceptable argument for a move destination.
(define_predicate "move_dest_operand"
  (match_code "reg, subreg, mem")
{
  switch (GET_CODE (op))
    {
    case REG :
     /* Program Counter register cannot be the target of a move.It is
	 a readonly register */
      if (REGNO (op) == PROGRAM_COUNTER_REGNO)
	return 0;
      else if (TARGET_MULMAC_32BY16_SET
	       && (REGNO (op) == 56 || REGNO(op) == 57))
	return 0;
      else if (TARGET_MUL64_SET
	       && (REGNO (op) == 57 || REGNO(op) == 58 || REGNO(op) == 59 ))
	return 0;
      else
	return register_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	return address_operand (XEXP (SUBREG_REG (op), 0), mode);
      else
	return register_operand (op, mode);
    case MEM :
      {
	rtx addr = XEXP (op, 0);

	if (GET_CODE (addr) == PLUS
	    && (GET_CODE (XEXP (addr, 0)) == MULT
		|| (!CONST_INT_P (XEXP (addr, 1))
		    && (TARGET_NO_SDATA_SET
			|| GET_CODE (XEXP (addr, 1)) != SYMBOL_REF
			|| !SYMBOL_REF_SMALL_P (XEXP (addr, 1))))))
	  return 0;
	if ((GET_CODE (addr) == PRE_MODIFY || GET_CODE (addr) == POST_MODIFY)
	    && (GET_CODE (XEXP (addr, 1)) != PLUS
		|| !CONST_INT_P (XEXP (XEXP (addr, 1), 1))))
	  return 0;
	return address_operand (addr, mode);
      }
    default :
      return 0;
    }

}
)

;; Return true if OP is valid load with update operand.
(define_predicate "load_update_operand"
  (match_code "mem")
{
  if (GET_CODE (op) != MEM
      || GET_MODE (op) != mode)
    return 0;
  op = XEXP (op, 0);
  if (GET_CODE (op) != PLUS
      || GET_MODE (op) != Pmode
      || !register_operand (XEXP (op, 0), Pmode)
      || !nonmemory_operand (XEXP (op, 1), Pmode))
    return 0;
  return 1;

}
)

;; Return true if OP is valid store with update operand.
(define_predicate "store_update_operand"
  (match_code "mem")
{
  if (GET_CODE (op) != MEM
      || GET_MODE (op) != mode)
    return 0;
  op = XEXP (op, 0);
  if (GET_CODE (op) != PLUS
      || GET_MODE (op) != Pmode
      || !register_operand (XEXP (op, 0), Pmode)
      || !(GET_CODE (XEXP (op, 1)) == CONST_INT
	   && SMALL_INT (INTVAL (XEXP (op, 1)))))
    return 0;
  return 1;
}
)

;; Return true if OP is a non-volatile non-immediate operand.
;; Volatile memory refs require a special "cache-bypass" instruction
;; and only the standard movXX patterns are set up to handle them.
(define_predicate "nonvol_nonimm_operand"
  (and (match_code "subreg, reg, mem")
       (match_test "(GET_CODE (op) != MEM || !MEM_VOLATILE_P (op)) && nonimmediate_operand (op, mode)"))
)

;; Accept integer operands in the range -0x80000000..0x7fffffff.  We have
;; to check the range carefully since this predicate is used in DImode
;; contexts.
(define_predicate "const_sint32_operand"
  (match_code "const_int")
{
  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) >= (-0x7fffffff - 1) && INTVAL (op) <= 0x7fffffff));
}
)

;; Accept integer operands in the range 0..0xffffffff.  We have to check the
;; range carefully since this predicate is used in DImode contexts.  Also, we
;; need some extra crud to make it work when hosted on 64-bit machines.
(define_predicate "const_uint32_operand"
  (match_code "const_int, const_double")
{
#if HOST_BITS_PER_WIDE_INT > 32
  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) >= 0 && INTVAL (op) <= 0xffffffffL));
#else
  return ((GET_CODE (op) == CONST_INT && INTVAL (op) >= 0)
	  || (GET_CODE (op) == CONST_DOUBLE && CONST_DOUBLE_HIGH (op) == 0));
#endif
}
)

;; Return 1 if OP is a comparison operator valid for the mode of CC.
;; This allows the use of MATCH_OPERATOR to recognize all the branch insns.

(define_predicate "proper_comparison_operator"
  (match_code "eq, ne, le, lt, ge, gt, leu, ltu, geu, gtu, unordered, ordered, uneq, unge, ungt, unle, unlt, ltgt")
{
  enum rtx_code code = GET_CODE (op);

  if (!COMPARISON_P (op))
    return 0;

  /* After generic flag-setting insns, we can use eq / ne / pl / mi / pnz .
     There are some creative uses for hi / ls after shifts, but these are
     hard to understand for the compiler and could be at best the target of
     a peephole.  */
  switch (GET_MODE (XEXP (op, 0)))
    {
    case CC_ZNmode:
      return (code == EQ || code == NE || code == GE || code == LT
	      || code == GT);
    case CC_Zmode:
      return code == EQ || code == NE;
    case CC_Cmode:
      return code == LTU || code == GEU;
    case CC_FP_GTmode:
      return code == GT || code == UNLE;
    case CC_FP_GEmode:
      return code == GE || code == UNLT;
    case CC_FP_ORDmode:
      return code == ORDERED || code == UNORDERED;
    case CC_FP_UNEQmode:
      return code == UNEQ || code == LTGT;
    case CC_FPXmode:
      return (code == EQ || code == NE || code == UNEQ || code == LTGT
	      || code == ORDERED || code == UNORDERED);
    case CCmode:
    case SImode: /* Used for BRcc.  */
      return 1;
    /* From combiner.  */
    case QImode: case HImode: case DImode: case SFmode: case DFmode:
      return 0;
    case VOIDmode:
      return 0;
    default:
      gcc_unreachable ();
  }
})

;; Return TRUE if this is the condition code register, if we aren't given
;; a mode, accept any CCmode register
(define_special_predicate "cc_register"
  (match_code "reg")
{
  if (mode == VOIDmode)
    {
      mode = GET_MODE (op);
      if (GET_MODE_CLASS (mode) != MODE_CC)
        return FALSE;
    }

  if (mode == GET_MODE (op) && GET_CODE (op) == REG && REGNO (op) == 61)
    return TRUE;

  return FALSE;
})

(define_predicate "zn_compare_operator"
  (match_code "compare")
{
  return GET_MODE (op) == CC_ZNmode || GET_MODE (op) == CC_Zmode;
})

;; Return true if OP is a shift operator.
(define_predicate "shift_operator"
  (match_code "ashiftrt, lshiftrt, ashift")
)

(define_predicate "commutative_operator"
  (ior (match_code "plus,ior,xor,and")
       (and (match_code "mult") (match_test "TARGET_ARC700"))
       (and (match_code "ss_plus")
	    (match_test "TARGET_ARC700 || TARGET_EA_SET")))
)

(define_predicate "commutative_operator_sans_mult"
  (ior (match_code "plus,ior,xor,and")
       (and (match_code "ss_plus")
	    (match_test "TARGET_ARC700 || TARGET_EA_SET")))
)

(define_predicate "mult_operator"
    (and (match_code "mult") (match_test "TARGET_ARC700"))
)

(define_predicate "noncommutative_operator"
  (ior (match_code "minus,ashift,ashiftrt,lshiftrt,rotatert")
       (and (match_code "ss_minus")
	    (match_test "TARGET_ARC700 || TARGET_EA_SET")))
)

(define_predicate "unary_operator"
  (ior (match_code "abs,neg,not,sign_extend,zero_extend")
       (and (ior (match_code "ss_neg")
		 (and (match_code "ss_truncate")
		      (match_test "GET_MODE (XEXP (op, 0)) == HImode")))
	    (match_test "TARGET_ARC700 || TARGET_EA_SET")))
)

(define_predicate "_2_4_8_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 2 || INTVAL (op) == 4 || INTVAL (op) == 8"))
)

(define_predicate "arc_double_register_operand"
  (match_code "reg")
{
  if ((GET_MODE (op) != mode) && (mode != VOIDmode))
    return 0;
  
  return (GET_CODE (op) == REG
		   && (REGNO (op) >= FIRST_PSEUDO_REGISTER
			     || REGNO_REG_CLASS (REGNO (op)) == DOUBLE_REGS));
})

(define_predicate "shouldbe_register_operand"
  (match_code "reg,subreg,mem")
{
  return ((reload_in_progress || reload_completed)
	  ? general_operand : register_operand) (op, mode);
})

(define_predicate "vector_register_operand"
  (match_code "reg")
{
  if ((GET_MODE (op) != mode) && (mode != VOIDmode))
  return 0;
  
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) == SIMD_VR_REGS));
})

(define_predicate "vector_register_or_memory_operand"
  ( ior (match_code "reg")
	(match_code "mem"))
{
  if ((GET_MODE (op) != mode) && (mode != VOIDmode))
    return 0;

  if ((GET_CODE (op) == MEM) 
      && (mode == V8HImode)
      && GET_CODE (XEXP (op,0)) == REG)
    return 1;
  
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) == SIMD_VR_REGS));
})

(define_predicate "arc_dpfp_operator"
  (match_code "plus, mult,minus")
)

(define_predicate "arc_simd_dma_register_operand"
  (match_code "reg")
{
  if ((GET_MODE (op) != mode) && (mode != VOIDmode))
    return 0;
  
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) == SIMD_DMA_CONFIG_REGS));
})

(define_predicate "acc1_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == (TARGET_BIG_ENDIAN ? 56 : 57)")))
  
(define_predicate "acc2_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == (TARGET_BIG_ENDIAN ? 57 : 56)")))
  
(define_predicate "mlo_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == (TARGET_BIG_ENDIAN ? 59 : 58)")))
  
(define_predicate "mhi_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == (TARGET_BIG_ENDIAN ? 58 : 59)")))
