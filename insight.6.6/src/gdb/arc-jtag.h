/* Target dependent code for ARC700, for GDB, the GNU debugger.

   Copyright 2005 Free Software Foundation, Inc.

   Contributed by Codito Technologies Pvt. Ltd. (www.codito.com)

   Authors:
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
/*     This header file defines some operations provided by the ARC JTAG      */
/*     module.                                                                */
/*                                                                            */
/******************************************************************************/

#ifndef ARC_JTAG_H
#define ARC_JTAG_H

#include "arc-support.h"


#define ARC_TARGET_OBJECT_AUXREGS 	(-1)


#define target_read_aux_reg(readbuf, first_regno, count)           \
         current_target.to_xfer_partial(&current_target,           \
                                        ARC_TARGET_OBJECT_AUXREGS, \
                                        NULL,                      \
                                        (gdb_byte*) readbuf,       \
                                        NULL,                      \
                                        first_regno,               \
                                        count)

#define target_write_aux_reg(writebuf, first_regno, count)         \
         current_target.to_xfer_partial(&current_target,           \
                                        ARC_TARGET_OBJECT_AUXREGS, \
                                        NULL,                      \
                                        NULL,                      \
                                        (gdb_byte*) writebuf,      \
                                        first_regno,               \
                                        count)


void _initialize_arc_debug(void);


/* utility functions */

ARC_RegisterContents arc_clear_status32_user_bit (void);
void                 arc_restore_status32_user_bit (ARC_RegisterContents status32);

Boolean arc_read_aux_register  (ARC_RegisterNumber hwregno, ARC_RegisterContents* contents);
Boolean arc_write_aux_register (ARC_RegisterNumber hwregno, ARC_RegisterContents  contents);

#endif /* ARC_JTAG_H */
/******************************************************************************/
