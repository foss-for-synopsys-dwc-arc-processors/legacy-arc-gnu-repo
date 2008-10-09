/* Target dependent code for ARC700, for GDB, the GNU debugger.

   Copyright 2005 Free Software Foundation, Inc.

   Contributed by Codito Technologies Pvt. Ltd. (www.codito.com)

   Authors: 
      Sameer Dhavale <sameer.dhavale@codito.com>
      Soam Vasani <soam.vasani@codito.com>

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
/*     This header file defines the JTAG interface to an ARC processor.       */
/*                                                                            */
/*     Operations are provided for:                                           */
/*        1) controlling the interface                                        */
/*        2) reading/writing the core registers of the processor              */
/*        3) reading/writing the auxiliary registers of the processor         */
/*        4) reading/writing single words in the target memory                */
/*        5) reading/writing blocks in the target memory                      */
/*        6) discovering the architectural version of the ARC processor       */
/*                                                                            */
/*     The addresses specified for the memory word read/write operations must */
/*     be word-aligned.  Those specified for the memory block read/write      */
/*     operations may have any alignment; these operations may transfer an    */
/*     arbitrary number of bytes.                                             */
/*                                                                            */
/* Usage:                                                                     */
/*     The module exports a global variable arc_jtag_ops which holds pointers */
/*     to the functions for the operations, as well as some state information.*/
/*     This variable is initialized by the function _initialize_arc_jtag_ops  */
/*     which must be called before any use is made of the module (N.B. the    */
/*     call to this function is generated by the gdb build mechanism, so this */
/*     function should not be explicitly called).                             */
/*                                                                            */
/*     The variable arc_jtag_ops.jtag_retry_count controls how many repeat    */
/*     attempts are made if a read/write operation fail; this variable is     */
/*     initially set to 50.                                                   */
/*                                                                            */
/* Debugging facilities:                                                      */
/*     If the variable arc_jtag_ops.jtag_state_machine_debug is set to TRUE   */
/*     trace information will be output.                                      */
/*                                                                            */
/******************************************************************************/

#ifndef ARC_JTAG_OPS
#define ARC_JTAG_OPS

#include "arc-support.h"


#define ARC_TARGET_NAME     "arcjtag"

#define GPIO_DEVICE         "/dev/gpio"


#define RAUX(name, hwregno, desc, gdbregno, mask, mode, version)  ARC_HW_##name##_REGNUM = hwregno,
#define RBCR(name, hwregno, desc, gdbregno, mask, mode, version)  ARC_HW_##name##_REGNUM = hwregno,

typedef enum
{
#include "arc-regnums-defs.h"

    /* Specific ARCangel Registers for Caches.  */
    ARC_HW_ICACHE_IVIC    = 0x10, /* Invalidate Cache. */
    ARC_HW_ICACHE_CONTROL = 0x11, /* Disable ICache. ICache control. */
    ARC_HW_DCACHE_IVIC    = 0x47, /* Invalidate Cache. */
    ARC_HW_DCACHE_CONTROL = 0x48, /* Disable DCache. DCache Control. */
} ARC_HardwareRegisters;

#undef RBCR
#undef RAUX


typedef enum
{
    JTAG_SUCCESS,
    JTAG_READ_FAILURE,
    JTAG_WRITE_FAILURE
} JTAG_OperationStatus;


typedef enum
{
    JTAG_OPENED,
    JTAG_CLOSED
} JTAG_Status;


typedef enum
{
    UNSUPPORTED,
    ARCompact,
    ARC600,
    ARC700,
    A5,
    A4
} ARC_ProcessorVersion;


typedef struct
{
    char*        name;
    JTAG_Status  jtag_status;
    unsigned int jtag_retry_count;
    Boolean      jtag_state_machine_debug;

    Boolean (*jtag_open)        (void);
    void    (*jtag_close)       (void);
    void    (*jtag_wait)        (void);
    void    (*jtag_reset_board) (void);

    /* these operations return the number of bytes read/written */
    unsigned int (*jtag_memory_read_word)     (ARC_Address addr, ARC_Word* data);                     // single word
    unsigned int (*jtag_memory_write_word)    (ARC_Address addr, ARC_Word  data);                     // single word
    unsigned int (*jtag_memory_read_chunk)    (ARC_Address addr, ARC_Byte* data,    unsigned int len);   // block
    unsigned int (*jtag_memory_write_chunk)   (ARC_Address addr, ARC_Byte* data,    unsigned int len);   // block
    unsigned int (*jtag_memory_write_pattern) (ARC_Address addr, ARC_Word  pattern, unsigned int len);   // block

    /* an operation to fill a chunk of memory with zeroes */
#define jtag_memory_zero_fill(addr, len)     jtag_memory_write_pattern(addr, 0, len)

    JTAG_OperationStatus (*jtag_read_aux_reg)   (ARC_RegisterNumber reg, ARC_RegisterContents* contents);
    JTAG_OperationStatus (*jtag_write_aux_reg)  (ARC_RegisterNumber reg, ARC_RegisterContents  contents);
    JTAG_OperationStatus (*jtag_read_core_reg)  (ARC_RegisterNumber reg, ARC_RegisterContents* contents);
    JTAG_OperationStatus (*jtag_write_core_reg) (ARC_RegisterNumber reg, ARC_RegisterContents  contents);
} JTAG_Operations;


extern JTAG_Operations arc_jtag_ops;


void _initialize_arc_jtag_ops (void);


ARC_ProcessorVersion arc_get_architecture(void);

#define IS_ARC700	(arc_get_architecture() == ARC700)
#define IS_ARC600	(arc_get_architecture() == ARC600)
#define IS_A5		(arc_get_architecture() == A5)
#define IS_A4		(arc_get_architecture() == A4)

#endif /* ARC_JTAG_OPS */
/******************************************************************************/