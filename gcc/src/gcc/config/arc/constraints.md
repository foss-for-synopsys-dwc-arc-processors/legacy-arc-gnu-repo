;; Constraint definitions for ARC.
;; Copyright (C) 2007 Celunite, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Register constraints

; Most instructions accept arbitrary core registers for their inputs, even
; if the core register in question cannot be written to, like the multiply
; result registers of the ARCtangent-A5 and ARC600
(define_register_constraint "c" "CORE_REGS"
  "core register @code{r0}-@code{r60}, @code{ap},@code{pcl}")

; Some core registers (.e.g lp_count) aren't general registers because they
; can't be used as the destination of a multi-cycle operation like
; load and/or multiply, yet they are still writable in the sense that
; register-register moves and single-cycle arithmetic (e.g "add", "and",
; but not "mpy") can write to them.
(define_register_constraint "w" "WRITABLE_CORE_REGS"
  "writable core register: @code{r0}-@code{r31}, @code{r60}, nonfixed core register")

(define_register_constraint "l" "LPCOUNT_REG"
  "@internal
   Loop count register @code{r60}")

(define_register_constraint "x" "R0_REG"
  "@code{R0} register.")

(define_register_constraint "Rgp" "GP_REG"
  "@internal
   Global Pointer register @code{r26}")

(define_register_constraint "f" "FP_REG"
  "@internal
   Frame Pointer register @code{r27}")

(define_register_constraint "b" "SP_REG"
  "@internal
   Stack Pointer register @code{r28}")

(define_register_constraint "k" "LINK_REGS"
  "@internal
   Link Registers @code{ilink1}:@code{r29}, @code{ilink2}:@code{r30},
   @code{blink}:@code{r31},")

(define_register_constraint "q" "ARCOMPACT16_REGS"
  "Registers usable in ARCompact 16-bit instructions: @code{r0}-@code{r3},
   @code{r12}-@code{r15}")

(define_register_constraint "e" "AC16_BASE_REGS"
  "Registers usable as base-regs of memory addresses in ARCompact 16-bit memory
   instructions: @code{r0}-@code{r3}, @code{r12}-@code{r15}, @code{sp}")

(define_register_constraint "D" "DOUBLE_REGS"
  "ARC FPX (dpfp) 64-bit registers. @code{D0}, @code{D1}")

(define_register_constraint "d" "SIMD_DMA_CONFIG_REGS"
  "@internal
   ARC SIMD DMA configuration registers @code{di0}-@code{di7},
   @code{do0}-@code{do7}")

(define_register_constraint "v" "SIMD_VR_REGS"
  "ARC SIMD 128-bit registers @code{VR0}-@code{VR23}")

; We could allow call-saved registers for sibling calls if we restored them
; in the delay slot of the call.  However, that would not allow to adjust the
; stack pointer afterwards, so the call-saved register would have to be
; restored from a call-used register that was just loaded with the value
; before.  So sticking to call-used registers for sibcalls will likely
; generate better code overall.
(define_register_constraint "Rsc" "SIBCALL_REGS"
  "@internal
   Sibling call register")

;; Integer constraints

(define_constraint "I"
  "@internal
   For ARCtangent-A4, an integer constant in the range -256 to 255. For other
   ARC cores a signed 12-bit integer constant." 
  (and (match_code "const_int")
       (match_test "(TARGET_A4 ? SMALL_INT (ival) : SIGNED_INT12 (ival))")))

(define_constraint "J"
  "@internal
   A 32-bit signed integer constant"
  (and (match_code "const_int")
       (match_test "LARGE_INT (ival)")))

(define_constraint "K"
  "@internal
   A 3-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT3 (ival)")))

(define_constraint "L"
  "@internal
   A 6-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT6 (ival)")))

(define_constraint "M"
  "@internal
   A 5-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT5 (ival)")))

(define_constraint "N"
  "@internal
   Integer constant 1"
  (and (match_code "const_int")
       (match_test "IS_ONE (ival)")))

(define_constraint "O"
  "@internal
   A 7-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT7 (ival)")))

(define_constraint "P"
  "@internal
   A 8-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT8 (ival)")))

(define_constraint "C_0"
  "@internal
   Zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "Cca"
  "@internal
   Conditional add / sub constant"
  (and (match_code "const_int")
       (match_test "ival == -1 << 31
		    || (ival >= -0x1f8 && ival <= 0x1f8
			&& ((ival >= 0 ? ival : -ival)
			    <= 0x3f * (ival & -ival)))")))

(define_constraint "C2a"
  "@internal
   Unconditional two-address add / sub constant"
  (and (match_code "const_int")
       (match_test "ival == -1 << 31
		    || (ival >= -0x4000 && ival <= 0x4000
			&& ((ival >= 0 ? ival : -ival)
			    <= 0x7ff * (ival & -ival)))")))

;; Floating-point constraints

(define_constraint "G"
  "@internal
   A 32-bit constant double value"
  (and (match_code "const_double")
       (match_test "arc_double_limm_p (op)")))

(define_constraint "H"
  "@internal
   All const_double values (including 64-bit values)"
  (and (match_code "const_double")
       (match_test "1")))

;; Memory constraints
(define_memory_constraint "T"
  "@internal
   A valid memory operand for ARCompact load instructions"
  (and (match_code "mem")
       (match_test "compact_load_memory_operand (op, VOIDmode)")))

(define_memory_constraint "S"
  "@internal
   A valid memory operand for ARCompact store instructions"
  (and (match_code "mem")
       (match_test "compact_store_memory_operand (op, VOIDmode)")))

(define_memory_constraint "Usd"
  "@internal
   A valid _small-data_ memory operand for ARCompact instructions"
  (and (match_code "mem")
       (match_test "compact_sda_memory_operand (op, VOIDmode)")))

(define_memory_constraint "Usc"
  "@internal
   A valid memory operand for storing constants"
  (and (match_code "mem")
       (match_test "!CONSTANT_P (XEXP (op,0))")
;; ??? the assembler rejects stores of immediates to small data.
       (match_test "!compact_sda_memory_operand (op, VOIDmode)")))

;; General constraints

(define_constraint "Cbr"
  "Branch destination"
  (ior (and (match_code "symbol_ref")
	   (match_test "!TARGET_LONG_CALLS_SET
			|| ARC_ENCODED_SHORT_CALL_ATTR_P (XSTR (op, 0))"))
      (match_code "label_ref")))

(define_constraint "Cpc"
  "pc-relative constant"
  (match_test "arc_legitimate_pc_offset_p (op)"))

(define_constraint "Cal"
  "constant for arithmetic/logical operations"
  (match_test "immediate_operand (op, VOIDmode) && !arc_legitimate_pc_offset_p (op)"))

(define_constraint "Rcq"
  "@internal
   Cryptic q - for short insn generation while not affecting register allocation
   Registers usable in ARCompact 16-bit instructions: @code{r0}-@code{r3},
   @code{r12}-@code{r15}"
  (and (match_code "REG")
       (match_test "arc_size_opt_level >= 1
		    && !arc_cond_exec_p ()
		    && ((((REGNO (op) & 7) ^ 4) - 4) & 15) == REGNO (op)")))

(define_constraint "Rs5"
  "@internal
   sibcall register - only allow one of the five available 16 bit isnsn.
   Registers usable in ARCompact 16-bit instructions: @code{r0}-@code{r3},
   @code{r12}"
  ; since this branch ends the function, there is no need to worry about
  ; slowing down the program by misalignment - so don't check
  ; arc_size_opt_level.
  (and (match_code "REG")
       (match_test "!arc_cond_exec_p ()")
       (ior (match_test "(unsigned) REGNO (op) <= 3")
	    (match_test "REGNO (op) == 12"))))


(define_constraint "Q"
  "@internal
   Integer constant zero"
  (and (match_code "const_int")
       (match_test "IS_ZERO (ival)")))
