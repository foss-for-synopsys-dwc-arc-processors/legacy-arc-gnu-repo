/******************************************************************************
 * Copyright Codito Technologies (www.codito.com) Oct 01, 2004
 * 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 *****************************************************************************/

/* vi: set sw=4 ts=4: */
/* i386 ELF shared library loader suppport
 *
 * Copyright (c) 1994-2000 Eric Youngdale, Peter MacDonald, 
 *				David Engel, Hongjiu Lu and Mitch D'Souza
 * Copyright (C) 2001-2002, Erik Andersen
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. The name of the above contributors may not be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/* Program to load an ELF binary on a linux system, and run it.
   References to symbols in sharable libraries can be resolved by either
   an ELF sharable library or a linux style of shared library. */

/* Disclaimer:  I have never seen any AT&T source code for SVr4, nor have
   I ever taken any courses on internals.  This program was developed using
   information available through the book "UNIX SYSTEM V RELEASE 4,
   Programmers guide: Ansi C and Programming Support Tools", which did
   a more than adequate job of explaining everything required to get this
   working. */

#include "ldso.h"

extern int _dl_linux_resolve(void);

unsigned long _dl_linux_resolver(struct elf_resolve *tpnt, int reloc_entry)
{
	int reloc_type;
	ELF_RELOC *this_reloc;
	char *strtab;
	Elf32_Sym *symtab;
	int symtab_index;
	char *rel_addr;
	char *new_addr;
	char **got_addr;
	unsigned long instr_addr;
	char *symname;
	unsigned long plt_got_base;
	rel_addr = (char *) (tpnt->dynamic_info[DT_JMPREL]);
	plt_got_base = tpnt->dynamic_info[DT_PLTGOT];
	/* The relocation entry is actually the pc. This should be subtracted 
	 * from the base of the PLT and divided by the size of the plt 
	 * entry . Also subtracting the size of PLT0 and adding a dummy PLT Entry 
	 * and right shifting by 4 (since the size of every plt entry is 0xc and the ceiling for
	 * a right shift would be 4.
	 */

	reloc_entry = ((reloc_entry - plt_got_base) / 0xc  - 2)* sizeof(ELF_RELOC) ;
	/* Should there not be a cleaner way of doing this.  */
	this_reloc = (ELF_RELOC *)(intptr_t)(rel_addr + reloc_entry);
	reloc_type = ELF32_R_TYPE(this_reloc->r_info);
	symtab_index = ELF32_R_SYM(this_reloc->r_info);
	symtab = (Elf32_Sym *)(intptr_t) (tpnt->dynamic_info[DT_SYMTAB]);
	strtab = (char *) (tpnt->dynamic_info[DT_STRTAB]);
	symname= strtab + symtab[symtab_index].st_name;

	if (reloc_type != R_ARC_JMP_SLOT) {
		_dl_dprintf(2, "%s: Incorrect relocation type in jump relocations\n", 
				_dl_progname);
		_dl_exit(1);
	}

	/* Address of jump instruction to fix up */
	instr_addr = ((unsigned long) this_reloc->r_offset + 
			(unsigned long) tpnt->loadaddr);
	got_addr = (char **) instr_addr;

	/* Get the address of the GOT entry */
	new_addr = _dl_find_hash(symname, tpnt->symbol_scope, tpnt, ELF_RTYPE_CLASS_PLT);
	if (!new_addr) {
		new_addr = _dl_find_hash(symname, NULL, NULL, ELF_RTYPE_CLASS_PLT);
		if (new_addr) {
			return (unsigned long) new_addr;
		}
		_dl_dprintf(2, "%s: can't resolve symbol '%s'\n", _dl_progname, symname);
		_dl_exit(1);
	}


#if defined (__SUPPORT_LD_DEBUG__)
	if ((unsigned long) got_addr < 0x40000000)
	{
		if (_dl_debug_bindings)
		{
			_dl_dprintf(_dl_debug_file, "\nresolve function: %s", symname);
			if(_dl_debug_detail) _dl_dprintf(_dl_debug_file, 
					"\n\tpatched %x ==> %x @ %x\n", *got_addr, new_addr, got_addr);
		}
	}
	if (!_dl_debug_nofixups) {
		*got_addr = new_addr;
	}
#else
	*got_addr = new_addr;
#endif
	return (unsigned long) new_addr;
}

static int
_dl_parse(struct elf_resolve *tpnt, struct dyn_elf *scope,
	  unsigned long rel_addr, unsigned long rel_size,
	  int (*reloc_fnc) (struct elf_resolve *tpnt, struct dyn_elf *scope,
			    ELF_RELOC *rpnt, Elf32_Sym *symtab, char *strtab))
{
	unsigned int i;
	char *strtab;
	Elf32_Sym *symtab;
	ELF_RELOC *rpnt;
	int symtab_index;

	/* Now parse the relocation information */
	rpnt = (ELF_RELOC *)(intptr_t) (rel_addr);
	rel_size = rel_size / sizeof(ELF_RELOC);

	symtab = (Elf32_Sym *)(intptr_t) (tpnt->dynamic_info[DT_SYMTAB]);
	strtab = (char *) (tpnt->dynamic_info[DT_STRTAB]);

	  for (i = 0; i < rel_size; i++, rpnt++) {
	        int res;
	    
		symtab_index = ELF32_R_SYM(rpnt->r_info);
		
		/* When the dynamic linker bootstrapped itself, it resolved some symbols.
		   Make sure we do not do them again */
		if (!symtab_index && tpnt->libtype == program_interpreter)
			continue;
		if (symtab_index && tpnt->libtype == program_interpreter &&
		    _dl_symbol(strtab + symtab[symtab_index].st_name))
			continue;

#if defined (__SUPPORT_LD_DEBUG__)
		debug_sym(symtab,strtab,symtab_index);
		debug_reloc(symtab,strtab,rpnt);
#endif

		res = reloc_fnc (tpnt, scope, rpnt, symtab, strtab);

		if (res==0) continue;

		  
		if (res <0)
		{
		        int reloc_type = ELF32_R_TYPE(rpnt->r_info);
#if defined (__SUPPORT_LD_DEBUG__)
			_dl_dprintf(2, "can't handle reloc type %s\n ", _dl_reltypes(reloc_type));
#else
			_dl_dprintf(2, "can't handle reloc type %x\n", reloc_type);
#endif			
			_dl_exit(-res);
		}
		else if (res >0)
		{
			return res;
		}
	  }
	  return 0;
}


static int
_dl_do_reloc (struct elf_resolve *tpnt,struct dyn_elf *scope,
	      ELF_RELOC *rpnt, Elf32_Sym *symtab, char *strtab)
{
	int reloc_type;
	int symtab_index;
	char *symname;
	unsigned long *reloc_addr;
	unsigned long symbol_addr;
#if defined (__SUPPORT_LD_DEBUG__)
	unsigned long old_val;
	unsigned long new_val;
#endif

	reloc_addr   = (unsigned long *)(intptr_t) (tpnt->loadaddr + (unsigned long) rpnt->r_offset);
	reloc_type   = ELF32_R_TYPE(rpnt->r_info);
	symtab_index = ELF32_R_SYM(rpnt->r_info);
	symbol_addr  = 0;
	symname      = strtab + symtab[symtab_index].st_name;

	if (symtab_index) {

		symbol_addr = (unsigned long) _dl_find_hash(symname, scope, 
							    tpnt, elf_machine_type_class(reloc_type));

		/*
		 * We want to allow undefined references to weak symbols - this might
		 * have been intentional.  We should not be linking local symbols
		 * here, so all bases should be covered.
		 */

		if (!symbol_addr && ELF32_ST_BIND(symtab[symtab_index].st_info) == STB_GLOBAL) {
#if defined (__SUPPORT_LD_DEBUG__)
			_dl_dprintf(2, "\tglobal symbol '%s' already defined in '%s'\n",
					symname, tpnt->libname);
#endif
			return 0;
		}
	}

#if defined (__SUPPORT_LD_DEBUG__)
    {
       unsigned short *ptr = (unsigned short *)reloc_addr;
	   old_val = (unsigned long)*ptr++;
       old_val |= ((unsigned long)*ptr) << 16;
       new_val = old_val;
    }
#endif
		switch (reloc_type) {
			case R_ARC_NONE:
				break;
			case R_ARC_32:
		    {
                unsigned short *ptr = (unsigned short *)reloc_addr;
                unsigned long value = 0;
				value = (unsigned long)*ptr++;
                value |= ((unsigned long)*ptr) << 16;
				value += symbol_addr;
                *ptr-- = (unsigned short)(value >> 16);
                *ptr = (unsigned short)value; 
#if defined (__SUPPORT_LD_DEBUG__)
			    new_val = value;
#endif
				break;
			}
            case R_ARC_PC32:
            {
                unsigned short *ptr = (unsigned short *)reloc_addr;
                unsigned long value = 0;
				value = (unsigned long)*ptr++;
                value |= ((unsigned long)*ptr) << 16;
				value += symbol_addr - (unsigned long) reloc_addr;
                *ptr-- = (unsigned short)(value >> 16);
                *ptr = (unsigned short)value; 
#if defined (__SUPPORT_LD_DEBUG__)
			    new_val = value;
#endif
				break;
			}
            case R_ARC_GLOB_DAT:
			case R_ARC_JMP_SLOT:
			{
                unsigned short *ptr = (unsigned short *)reloc_addr;
                *ptr++ = (unsigned short)symbol_addr; 
                *ptr = (unsigned short)(symbol_addr >> 16);
#if defined (__SUPPORT_LD_DEBUG__)
			    new_val = symbol_addr;
#endif
				break;
            }
			case R_ARC_RELATIVE:
            {
                unsigned short *ptr = (unsigned short *)reloc_addr;
                unsigned long value = 0;
				value = (unsigned long)*ptr++;
                value |= ((unsigned long)*ptr) << 16;
                value += (unsigned long) tpnt->loadaddr;
                //_dl_debug_early("value is %x",(unsigned long)value);
                *ptr = (unsigned short)(value >> 16);
                ptr--;
                *ptr = (unsigned short)(value); 
#if defined (__SUPPORT_LD_DEBUG__)
			    new_val = value;
#endif
				break;
            }
			case R_ARC_COPY:
			        /* handled later on */
			        _dl_memcpy((void *) reloc_addr,
				         (void *) symbol_addr, symtab[symtab_index].st_size);
			        break;
		        default:

				return -1; /*call _dl_exit(1) */
		}
#if defined (__SUPPORT_LD_DEBUG__)
	if(_dl_debug_reloc && _dl_debug_detail)
		_dl_dprintf(_dl_debug_file, "\tpatched: %x ==> %x @ %x", old_val, new_val, reloc_addr);
#endif

	return 0;
}

static int
_dl_do_lazy_reloc (struct elf_resolve *tpnt, struct dyn_elf *scope,
		   ELF_RELOC *rpnt, Elf32_Sym *symtab, char *strtab)
{
	int reloc_type;
	unsigned long *reloc_addr;
#if defined (__SUPPORT_LD_DEBUG__)
	unsigned long old_val;
#endif
	(void)scope;
	(void)symtab;
	(void)strtab;

	reloc_addr = (unsigned long *)(intptr_t) (tpnt->loadaddr + (unsigned long) rpnt->r_offset);
	reloc_type = ELF32_R_TYPE(rpnt->r_info);

#if defined (__SUPPORT_LD_DEBUG__)
	old_val = *reloc_addr;
#endif
		switch (reloc_type) {
			case R_ARC_NONE:
				break;
			case R_ARC_JMP_SLOT:
				*reloc_addr += (unsigned long) tpnt->loadaddr;
				break;
			default:
				return -1; /*call _dl_exit(1) */
		}
#if defined (__SUPPORT_LD_DEBUG__)
	if(_dl_debug_reloc && _dl_debug_detail)
		_dl_dprintf(_dl_debug_file, "\tpatched: %x ==> %x @ %x", old_val, *reloc_addr, reloc_addr);
#endif
	return 0;

}
#if 0
/* This is done as a separate step, because there are cases where
   information is first copied and later initialized.  This results in
   the wrong information being copied.  Someone at Sun was complaining about
   a bug in the handling of _COPY by SVr4, and this may in fact be what he
   was talking about.  Sigh. */

/* No, there are cases where the SVr4 linker fails to emit COPY relocs
   at all */
static int
_dl_do_copy (struct elf_resolve *tpnt, struct dyn_elf *scope,
	     ELF_RELOC *rpnt, Elf32_Sym *symtab, char *strtab)
{
	int reloc_type;
	int symtab_index;
	unsigned long *reloc_addr;
	unsigned long symbol_addr;
	int goof = 0;
	char *symname;
	  
	reloc_addr = (unsigned long *)(intptr_t) (tpnt->loadaddr + (unsigned long) rpnt->r_offset);
	reloc_type = ELF32_R_TYPE(rpnt->r_info);
	if (reloc_type != R_ARC_COPY) 
		return 0;
	symtab_index = ELF32_R_SYM(rpnt->r_info);
	symbol_addr = 0;
	symname      = strtab + symtab[symtab_index].st_name;
		
	if (symtab_index) {
		symbol_addr = (unsigned long) _dl_find_hash(symname, scope, NULL, copyrel);
		if (!symbol_addr) goof++;
	}
	if (!goof) {
#if defined (__SUPPORT_LD_DEBUG__)
	        if(_dl_debug_move)
		  _dl_dprintf(_dl_debug_file,"\n%s move %x bytes from %x to %x",
			     symname, symtab[symtab_index].st_size,
			     symbol_addr, symtab[symtab_index].st_value);
#endif
		_dl_memcpy((char *) symtab[symtab_index].st_value, 
			(char *) symbol_addr, symtab[symtab_index].st_size);
	}

	return goof;
}
#endif

void _dl_parse_lazy_relocation_information(struct dyn_elf *rpnt, 
	unsigned long rel_addr, unsigned long rel_size)
{
	(void)_dl_parse(rpnt->dyn, NULL, rel_addr, rel_size, _dl_do_lazy_reloc);
}

int _dl_parse_relocation_information(struct dyn_elf  *rpnt, 
	unsigned long rel_addr, unsigned long rel_size)
{
	return _dl_parse(rpnt->dyn, rpnt->dyn->symbol_scope, rel_addr, rel_size, _dl_do_reloc);
}

#if 0
int _dl_parse_copy_information(struct dyn_elf *xpnt, unsigned long rel_addr, 
	unsigned long rel_size, int type)
{
	(void) type;
	return _dl_parse(xpnt->dyn, xpnt->next, rel_addr, rel_size, _dl_do_copy);
}

#endif
