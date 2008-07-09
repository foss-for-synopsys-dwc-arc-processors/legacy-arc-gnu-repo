#as: -EL -mARC700
#objdump: -dr -EL

.*: +file format elf32-.*arc

Disassembly of section .text:

00000000 <main>:
   0:	00 21 80 00 	21000080     add        r0,r1,r2
   4:	6f 25 3f 00 	256f003f     brk        
   8:	02 24 43 01 	24020143     sub        r3,r4,r5
