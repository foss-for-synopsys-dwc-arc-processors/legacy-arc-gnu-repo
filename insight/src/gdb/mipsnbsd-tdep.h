/* Common target dependent code for GDB on MIPS systems running NetBSD.

   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#ifndef MIPSNBSD_TDEP_H
#define MIPSNBSD_TDEP_H

void mipsnbsd_supply_reg (char *, int);
void mipsnbsd_fill_reg (char *, int);

void mipsnbsd_supply_fpreg (char *, int);
void mipsnbsd_fill_fpreg (char *, int);

#define SIZEOF_STRUCT_REG	(38 * mips_isa_regsize (current_gdbarch))
#define SIZEOF_STRUCT_FPREG	(33 * mips_isa_regsize (current_gdbarch))

#endif /* MIPSNBSD_TDEP_H */
