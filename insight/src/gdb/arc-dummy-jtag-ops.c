/* Target dependent code for ARC700, for GDB, the GNU debugger.

   Copyright 2008 Free Software Foundation, Inc.

   Contributed by ARC International (www.arc.com)

   Authors:
      Richard Stuckey <richard.stuckey@arc.com>

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
/*     This module implements a dummy set of operations for debug access to   */
/*     an ARC processor via its JTAG interface.                               */
/*                                                                            */
/*     It is useful for testing when no hardware target is available.         */
/*                                                                            */
/******************************************************************************/

/* system header files */
#include <stdio.h>

/* gdb header files */

/* ARC header files */
#include "arc-jtag-ops.h"


/* -------------------------------------------------------------------------- */
/*                               local types                                  */
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
/*                               local data                                   */
/* -------------------------------------------------------------------------- */

#define AP_BUILD_REG    0x76         // Actionpoints build register
#define PC_REG          0x6
#define DEBUG_REG       0x5
#define AMV0_REG        0x220
#define AMV1_REG        0x223


static unsigned int num_actionpoints = 8;


/* -------------------------------------------------------------------------- */
/*                               externally visible data                      */
/* -------------------------------------------------------------------------- */

JTAG_Operations arc_jtag_ops;


/* -------------------------------------------------------------------------- */
/*                               local macros                                 */
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
/*                               local functions                              */
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
/*                               main operations                              */
/* -------------------------------------------------------------------------- */

/* these are the functions that are called from outside this module via the
 * pointers in the arc_jtag_ops global object
 *
 * N.B. none of these functions are called from within this module
 */

/* Read a processor core register */
static JTAG_OperationStatus
jtag_read_core_reg(ARC_RegisterNumber regnum, ARC_RegisterContents* contents)
{
    return JTAG_SUCCESS;
}


/* Write a processor core register */
static JTAG_OperationStatus
jtag_write_core_reg(ARC_RegisterNumber regnum, ARC_RegisterContents contents)
{
    return JTAG_SUCCESS;
}


/* Read a processor auxiliary register */
static JTAG_OperationStatus
jtag_read_aux_reg(ARC_RegisterNumber regnum, ARC_RegisterContents* contents)
{
    if (regnum == PC_REG)
        *contents = 0x00001008;
    else if (regnum == AP_BUILD_REG)
    {
        if (num_actionpoints == 2)
            *contents = 0x00000004;
        else if (num_actionpoints == 4)
            *contents = 0x00000104;
        else
            *contents = 0x00000204;
    }
    else if (regnum == DEBUG_REG)
    {
        /* fake trigger of AP 1 */
        *contents = DEBUG_ACTIONPOINT_HALT |
                    (1 << (DEBUG_ACTIONPOINT_STATUS_SHIFT + 1));
    }
    else if (regnum == AMV1_REG)
    {
        *contents = 0x4008;
    }
    else
        *contents = 0;

//  DEBUG("regnum = %x, contents = 0x%08X", regnum, *contents);

    return JTAG_SUCCESS;
}


/* Write a processor auxiliary register */
static JTAG_OperationStatus
jtag_write_aux_reg(ARC_RegisterNumber regnum, ARC_RegisterContents contents)
{
//  printf("AUX: regnum = %d, contents = 0x%08X\n", regnum, contents);
    return JTAG_SUCCESS;
}


/* Read a word of data from memory; the given address must be word-aligned.
 * Returns number of bytes read.
 */
static unsigned int
jtag_read_word(ARC_Address addr, ARC_Word* data)
{
    *data = 0;
    return BYTES_IN_WORD;
}


/* Write a word of data to memory; the given address must be word-aligned.
 * Returns number of bytes written.
 */
static unsigned int
jtag_write_word(ARC_Address addr, ARC_Word data)
{
    return BYTES_IN_WORD;
}


/* Read a chunk of data from target memory.
 * Returns number of bytes read.
 */
static unsigned int
jtag_read_chunk(ARC_Address addr, ARC_Byte* data, unsigned int bytes)
{
    return bytes;
}


/* Write a chunk of data to target memory.
 * data is NULL if the target memory is to be zero-filled
 * Returns number of bytes written.
 */
static unsigned int
jtag_write_chunk(ARC_Address addr, ARC_Byte* data, unsigned int bytes)
{
    return bytes;
}


static unsigned int
jtag_write_pattern(ARC_Address addr, ARC_Word pattern, unsigned int bytes)
{
    return bytes;
}


/* Open the JTAG interface.
 * Returns TRUE for success.
 */
static Boolean
jtag_open(void)
{
    arc_jtag_ops.jtag_status = JTAG_OPENED;
    return TRUE;
}


/* close the JTAG interface */
static void
jtag_close(void)
{
    arc_jtag_ops.jtag_status = JTAG_CLOSED;
}


/* wait for the target to halt */
static void
jtag_wait(void)
{
    printf("*** target has halted!\n");
}


/* reset the target board */
static void
jtag_reset_board(void)
{
}


/* -------------------------------------------------------------------------- */
/*                               externally visible functions                 */
/* -------------------------------------------------------------------------- */

/* return the processor variant that is connected */
ARC_ProcessorVersion arc_get_architecture(void)
{
   return ARC700;
}


/* initialize the arc_jtag_ops global variable */
void
_initialize_arc_jtag_ops(void)
{
    arc_jtag_ops.name                     = NULL;
    arc_jtag_ops.jtag_status              = JTAG_CLOSED;
    arc_jtag_ops.jtag_state_machine_debug = FALSE;
    arc_jtag_ops.jtag_retry_count         = 50;

    arc_jtag_ops.jtag_open                 = jtag_open;
    arc_jtag_ops.jtag_close                = jtag_close;
    arc_jtag_ops.jtag_memory_read_word     = jtag_read_word;
    arc_jtag_ops.jtag_memory_write_word    = jtag_write_word;
    arc_jtag_ops.jtag_memory_read_chunk    = jtag_read_chunk;
    arc_jtag_ops.jtag_memory_write_chunk   = jtag_write_chunk;
    arc_jtag_ops.jtag_memory_write_pattern = jtag_write_pattern;
    arc_jtag_ops.jtag_read_aux_reg         = jtag_read_aux_reg;
    arc_jtag_ops.jtag_write_aux_reg        = jtag_write_aux_reg;
    arc_jtag_ops.jtag_read_core_reg        = jtag_read_core_reg;
    arc_jtag_ops.jtag_write_core_reg       = jtag_write_core_reg;
    arc_jtag_ops.jtag_wait                 = jtag_wait;
    arc_jtag_ops.jtag_reset_board          = jtag_reset_board;
}

/******************************************************************************/
