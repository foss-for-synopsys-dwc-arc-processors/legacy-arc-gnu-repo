/* Target dependent code for ARC700, for GDB, the GNU debugger.

   Copyright 2005 Free Software Foundation, Inc.

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
/*     This header file defines some useful types and constants, and macros   */
/*     for use in debugging.                                                  */
/*                                                                            */
/******************************************************************************/

#ifndef ARC_SUPPORT_H
#define ARC_SUPPORT_H


#define ARC_DEBUG 1

#if ARC_DEBUG
#define DEBUG(...)              if (arc_debug_target) printf_filtered(__VA_ARGS__)
#define ENTERMSG                DEBUG("--- entered %s:%s()\n",        __FILE__, __FUNCTION__)
#define ENTERARGS(fmt, args...) DEBUG("--- entered %s:%s(" fmt ")\n", __FILE__, __FUNCTION__, args)
#define LEAVEMSG                DEBUG("--- exited  %s:%s()\n",        __FILE__, __FUNCTION__)
#else
#define DEBUG(...)
#define ENTERMSG
#define ENTERARGS(fmt, args...)
#define LEAVEMSG
#endif


/* useful Boolean type and constants */
typedef int Boolean;

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif


typedef unsigned int  ARC_RegisterNumber;
typedef unsigned int  ARC_RegisterContents;
typedef unsigned int  ARC_Address;
typedef unsigned int  ARC_Word;
typedef unsigned char ARC_Byte;


/* sizes of quantities */
#define BYTES_IN_WORD              4
#define BYTES_IN_REGISTER          4
#define BITS_IN_BYTE               8
#define BITS_IN_WORD              32
#define BITS_IN_ADDRESS           32
#define BITS_IN_REGISTER          32


/* bit masks for use with the auxiliary DEBUG, IDENTITY, STATUS32 and
 * AP_BUILD registers
 */
#define DEBUG_SH                         0x40000000
#define DEBUG_USER                       0x10000000
#define DEBUG_ACTIONPOINT_HALT           0x00000004
#define DEBUG_ACTIONPOINT_STATUS         0x000007F8
#define DEBUG_ACTIONPOINT_STATUS_SHIFT   3
#define DEBUG_FORCE_HALT                 0x00000002
#define DEBUG_LOAD_PENDING               0x80000000
#define IDENTITY_ARCVER                  0x000000FF
#define STATUS32_USER                    0x00000080
#define STATUS32_L                       0x00000100
#define STATUS32_HALT                    0x00000001
#define AP_BUILD_VERSION                 0x000000FF
#define AP_BUILD_TYPE                    0x00000F00
#define AP_BUILD_TYPE_SHIFT              8


#define ELEMENTS_IN_ARRAY(arr)           (unsigned int) (sizeof(arr) / sizeof(arr[0]))


/* a global debug flag */
extern Boolean arc_debug_target;

#endif /* ARC_SUPPORT_H */
/******************************************************************************/
