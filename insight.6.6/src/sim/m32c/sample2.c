/* sample2.c --- main source for sample2.x test program for M32C simulator

Copyright (C) 2005, 2007 Free Software Foundation, Inc.
Contributed by Red Hat, Inc.

This file is part of the GNU simulators.

The GNU simulators are free software; you can redistribute them and/or
modify them under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU simulators are distributed in the hope that they will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with the GNU simulators; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA  */

/* See the 'sample2.x' target in Makefile.in.  */
void exit (int);

start ()
{
  foo (1, 2, 3, 4);
  exit (5);
}
