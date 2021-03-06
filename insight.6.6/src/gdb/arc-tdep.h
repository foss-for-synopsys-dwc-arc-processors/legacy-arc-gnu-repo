/* Target dependent code for ARC700, for GDB, the GNU debugger.

   Copyright 2005 Free Software Foundation, Inc.

   Contributed by Codito Technologies Pvt. Ltd. (www.codito.com)

   Authors: 
      Soam Vasani <soam.vasani@codito.com>
      Ramana Radhakrishnan <ramana.radhakrishnan@codito.com> 

   This file is part of GDB.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

/******************************************************************************/
/*                                                                            */
/* Outline:                                                                   */
/*     This header file defines some target-dependent information which is    */
/*     specific to the ARC gdb port.                                          */
/*                                                                            */
/******************************************************************************/

#ifndef ARC_TDEP_H
#define ARC_TDEP_H

#include "arc-support.h"
#include "arc-jtag-ops.h"


typedef enum
{
    ARC700_MMU
} ARC_ExtensionsSupportedInformation;


typedef struct ARCProcessorInformation
{
    ARC_ProcessorVersion               processor_version;
    ARC_ExtensionsSupportedInformation extensions_supported;  // not used
} ARC_VariantsInfo;


#define REGISTER_NOT_PRESENT   (-1)   // special value for sc_reg_offset[reg]


/* this structure hold target-dependent information
 *
 * N.B. this type is used in the target-independent gdb code, but it is treated
 *      as an opaque (or private) type: the only use of it is by pointers to
 *      objects of this type (passed as parameters or returned as results, or
 *      held in other structures); it is only the ARC-specific modules that
 *      have knowledge of the structure of this type and access its field.
 */
struct gdbarch_tdep
{
    /* Detect sigtramp.  */
    Boolean (*is_sigtramp) (struct frame_info*);
  
    /* Get address of sigcontext for sigtramp.  */
    CORE_ADDR (*sigcontext_addr) (struct frame_info*);

    /* Offset of registers in `struct sigcontext'. */
    const int*   sc_reg_offset;
    unsigned int sc_num_regs;

    /* In our linux target, gdbarch_pc_regnum points to stop_pc, which is a
       register that is made up by the kernel and does not actually exist.
       stop_pc is NOT saved in the sigcontext; what is saved is the ret
       "register".  Since ret is a linux-only register, its regnum is visible
       only in arc-linux-tdep.c; hence initialize pc_regnum_in_sigcontext in
       arc-linux-tdep.c.  */
    int pc_regnum_in_sigcontext;

    /* Returns 0, 1, or -1:
     *    0 means the register is not in the group.
     *    1 means the register is in the group.
     *   -1 means the tdep has nothing to say about this register and group.
     */
    int (*register_reggroup_p) (int regnum, struct reggroup* group);
  
    /* Names of processor registers, both real and pseudo */
    const char** register_names;
    unsigned int num_register_names;

    /* Breakpoint instruction to be used */
    const unsigned char* breakpoint_instruction;
    unsigned int         breakpoint_size;

    /* For stopping backtraces.  */
    CORE_ADDR lowest_pc;
  
    /* ARC processor variant information (may be NULL). */
    ARC_VariantsInfo* processor_variant_info;
};


void _initialize_arc_tdep (void);


/* utility functions used by other ARC-specific modules */

void arc_initialize_disassembler(struct disassemble_info* info);

int arc_binutils_reg_to_regnum (int reg);

#endif /* ARC_TDEP_H */
/******************************************************************************/
