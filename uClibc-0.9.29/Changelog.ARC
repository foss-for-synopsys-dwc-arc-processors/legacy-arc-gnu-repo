2011-06-21  Brendan Kehoe  <brendan@zen.org>

	* libc/sysdeps/linux/arc/mmap.c (mmap): Fix OFFSET shift in
	return to use __u_long instead of default signedness of off_t.

2008-09-05  Muhammad Khurram Riaz  <khurram.riaz@arc.com>

	* arc_config: Locale support enables
	* libc/string/arc/strcmp.S: features.h included
	* extra/locale/Makefile.in: header UCLIBC_CTYPE_HEADER path corrected
	* libc/sysdeps/linux/arc/bits/wchar.h: DELETED 


2008-08-27  J"orn Rennecke  <joern.rennecke@arc.com>

	* ldso/ldso/powerpc/dl-sysdep.h (ELF_RESOLVE_TDEP): Define.
	* ldso/ldso/dl-hash.c (_dl_elf_hash): Use implementation with
	tighter loop core.
	(_dl_add_elf_hash_tab): Take COPY_HASH into account.
	* ldso/ldso/arc/dl-sysdep.h (__udivmodsi4): Extern hidden declaration.
	(do_rem): Use __udivmodsi4.
	* ldso/include/dl-hash.h (struct elf_resolve): Use ELF_RESOLVE_TDEP
	instead of ad-hoc #ifdefs.

2008-07-22  J"orn Rennecke  <joern.rennecke@arc.com>

	* ldso/ldso/arc/resolve.S: Define __ASSEMBLY__
	* libc/sysdeps/linux/arc/vfork.S: Likewise.
	* libc/sysdeps/linux/arc/clone.S: Likewise.
	* libc/sysdeps/linux/arc/setjmp.S: Likewise.
	* libc/sysdeps/linux/arc/bsd-_setjmp.S: Likewise.
	* libc/sysdeps/linux/arc/bsd-setjmp.S: Likewise.
	* libc/sysdeps/linux/arc/crt1.S: Likewise.

2008-06-11  J"orn Rennecke  <joern.rennecke@arc.com>

	* libpthread/linuxthreads.old/sysdeps/arc/pt-machine.h:
	Only use register indirect or absolute addressing for ex.

2008-02-27  J"orn Rennecke  <joern.rennecke@arc.com>

	From Simon Spooner:
	* libc/string/arc/strcmp.S [__UCLIBC_HAS_LOCALE__] (strcoll): New alias.

2008-02-14  J"orn Rennecke  <joern.rennecke@arc.com>

	* libc/string/arc/uclibc-asm.h: New file.
	* libc/string/arc/memcmp.S: Include uclibc-asm.h.
	* libc/string/arc/strchr.S, libc/string/arc/strlen.S: Likewise.
	* libc/string/arc/strcmp.S, libc/string/arc/memcpy.S: Likewise.
	* libc/string/arc/memset.S: Likewise.
	* libc/string/arc/strcpy.S: Likewise.  Include newlib/strcpy-600.S.

2008-02-12  J"orn Rennecke  <joern.rennecke@arc.com>

	* libc/string/arc: New directory.

2007-12-06  J"orn Rennecke  <joern.rennecke@arc.com>

	* .config: New file, configuration for ARC.
	* extra/Configs/Config.arc: Adjust for 0.9.29.
	* include/libc-symbols.h (hidden_def): Define as hidden_weak.
	* Makerules: Be less silent.
	* ldso/ldso/arc/dl-startup.h: Updated for 0.9.29 (entire file).
	* ldso/ldso/arc/elfinterp.c: Updated for 0.9.29:
	(_dl_reltypes_tab) Remove.  (Btw this had incorrect indices.)
	(_dl_reltypes, debug_sym, debug_reloc): Remove.
	("ldso.h"): Include.
	* ldso/ldso/arc/dl-debug.h: New file.  Defines proper relocation
	names.
	* ldso/ldso/arc/dl-sysdep.h: Updated for 0.9.29:
	(elf.h): Include.
	(INIT_GOT): Fix cast type.
	(insert_arc_pc_rel_25): Cast REL argument to arc_read_me, arc_write_me
	and insert_arc_pc_rel_25.
	(elf_machine_dynamic): Fix gp name.
	(elf_machine_load_address): New function.
	(elf_machine_relative): New function.
	(libc/sysdeps/linux/arc/vfork.S): Adjust names to new scheme;
	provide weak aliases.
	* libc/sysdeps/linux/arc/brk.c (<unistd.h>): Include.
	(brk): Use libc_hidden_proto and libc_hidden_def.
	* libc/sysdeps/linux/arc/bits/uClibc_arch_features.h: New file.
	* libc/sysdeps/linux/arc/bits/syscalls.h: Include errno.h to
	get __set_errno definition.  Conditionalize this on
	#ifndef __ASSEMBLER__.
	* libc/sysdeps/linux/arc/bits/stackinfo.h: New file.
	* libc/sysdeps/linux/arc/__longjmp.S (__longjmp): Define.
	Add libc_hidden_def(__longjmp).
	* libc/sysdeps/linux/arc/crt0.S: Deleted.
	* libc/sysdeps/linux/arc/crt1.S: New file.
	* libc/sysdeps/linux/arc/Makefile: Updated for 0.9.29 (entire file).
	* libpthread/linuxthreads.old/sysdeps/arc/pt-machine.h: Copied from
	old linuxthreads/sysdeps/arc/pt-machine.h.

2007-10-24  J"orn Rennecke  <joern.rennecke@arc.com>

	* libc/sysdeps/linux/common/profil.c: New file.
