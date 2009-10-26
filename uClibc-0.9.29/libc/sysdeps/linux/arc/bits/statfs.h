/* Copyright (C) 1997, 1998, 2000, 2002, 2003 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#ifndef _SYS_STATFS_H
# error "Never include <bits/statfs.h> directly; use <sys/statfs.h> instead."
#endif

#include <bits/types.h>

struct statfs
  {
    __SWORD_TYPE f_type;
    __SWORD_TYPE f_bsize;
    __SWORD_TYPE f_frsize;
    __SWORD_TYPE __pad;

#ifndef __USE_FILE_OFFSET64
    __fsblkcnt_t f_blocks;
    __fsblkcnt_t f_bfree;
    __fsblkcnt_t f_bavail;
    __fsfilcnt_t f_files;
    __fsfilcnt_t f_ffree;

#else
    __fsblkcnt64_t f_blocks;
    __fsblkcnt64_t f_bfree;
    __fsfilcnt64_t f_files;
    __fsfilcnt64_t f_ffree;
    __fsblkcnt64_t f_bavail;
#endif
    __fsid_t f_fsid;
    __SWORD_TYPE f_namelen;
    __SWORD_TYPE f_spare[6];
  };

/* linux26 STRUCTURE (as in svn trunk as on 27 Aug 2008)
struct statfs {
	long		f_type;
	long		f_bsize;
	long		f_blocks;
	long		f_bfree;
	long		f_bavail;
	long		f_files;
	long		f_ffree;

	/* Linux specials 
	__kernel_fsid_t	f_fsid;
	long		f_namelen;
	long		f_spare[6];

	long		f_frsize;	/* Fragment size - unsupported 

};
*/
#ifdef __USE_LARGEFILE64
struct statfs64
  {
    __SWORD_TYPE f_type;
    __SWORD_TYPE f_bsize;
    __SWORD_TYPE f_frsize;
    __SWORD_TYPE __pad;
    __fsblkcnt64_t f_blocks;
    __fsblkcnt64_t f_bfree;
    __fsfilcnt64_t f_files;
    __fsfilcnt64_t f_ffree;
    __fsblkcnt64_t f_bavail;
    __fsid_t f_fsid;
    __SWORD_TYPE f_namelen;
    __SWORD_TYPE f_spare[6];
  };
#endif

/* linux26 STRUCTURE (as in svn trunk as on 27 Aug 2008)
struct statfs64 {
	__u32	f_type;
	__u32	f_bsize;
	__u32	f_frsize;	/* Fragment size - unsupported 
	__u32	__pad;
	__u64	f_blocks;
	__u64	f_bfree;
	__u64	f_files;
	__u64	f_ffree;
	__u64	f_bavail;
	__kernel_fsid_t f_fsid;
	__u32	f_namelen;
	__u32	f_spare[6];
};
*/
/* Tell code we have these members.  */
#define _STATFS_F_NAMELEN
#define _STATFS_F_FRSIZE
