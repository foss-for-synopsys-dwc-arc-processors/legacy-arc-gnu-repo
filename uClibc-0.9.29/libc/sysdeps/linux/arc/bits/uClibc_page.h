/******************************************************************************
 * Copyright Codito Technologies (www.codito.com) May 12, 2005
 * 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 *****************************************************************************/

/*
 *  sysdeps/linux/arc/bits/uClibc_page.h
 *
 *  Copyright (C) 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * Authors: Amit Bhor
 */
#ifndef _UCLIBC_PAGE_H
#define _UCLIBC_PAGE_H

/* PAGE_SHIFT determines the page size */
#define PAGE_SHIFT	13
#ifdef __ASSEMBLY__
#define PAGE_SIZE       (1 << PAGE_SHIFT)
#else
#define PAGE_SIZE       (1UL << PAGE_SHIFT)
#endif	/* __ASSEMBLY */
#define PAGE_MASK       (~(PAGE_SIZE-1))

#define MMAP2_PAGE_SHIFT PAGE_SHIFT

/* for ldso */
#define ADDR_ALIGN      (PAGE_SIZE -1)
#define PAGE_ALIGN      PAGE_MASK
#define OFFS_ALIGN      PAGE_MASK

#endif /* _UCLIBC_PAGE_H */
