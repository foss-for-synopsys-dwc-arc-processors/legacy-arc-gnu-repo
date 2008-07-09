2008-04-04  J"orn Rennecke  <joern.rennecke@arc.com>

	* gcc.c-torture/execute/ieee/denorm-rand.c: New file.
	* gcc.dg/torture/fp-int-convert.h: Avoid undefined behaviour.

2008-01-31  J"orn Rennecke  <joern.rennecke@arc.com>

	* gcc.dg/pr35044.c: New file.

2007-09-28  J"orn Rennecke  <joern.rennecke@arc.com>

	* gcc.c-torture/execute/20030125-1.x:
	arc-*-uclibc also doesn't support all of c99.

2007-04-25  J"orn Rennecke  <joern.rennecke@arc.com>

	* gcc.dg/sibcall-3.c: Don't XFAIL.
	* gcc.dg/sibcall-4.c: Likewise.

2007-04-25  J"orn Rennecke  <joern.rennecke@arc.com>

	* g++.dg/cpp/_Pragma1.c: Disable for arc-*-elf*.

2007-04-25  J"orn Rennecke  <joern.rennecke@arc.com>

	* gcc.dg/builtin-return-1.c (main): Make sure we have 64 bytes of
	stack to copy.

	* gcc.dg/cpp/_Pragma6.c: Disable for arc-*-elf*.

2007-04-24  J"orn Rennecke  <joern.rennecke@arc.com>

	* gcc.dg/builtin-apply2.c (main): Make sure we have 64 bytes of
	stack to copy.
