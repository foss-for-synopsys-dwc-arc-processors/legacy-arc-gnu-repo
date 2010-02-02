/* Target dependent code for ARC700, for GDB, the GNU debugger.

   Copyright 2005 Free Software Foundation, Inc.

   Contributed by Codito Technologies Pvt. Ltd. (www.codito.com)

   Authors:
      Soam Vasani <soam.vasani@codito.com>
      Ramana Radhakrishnan <ramana.radhakrishnan@codito.com>

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
/*     This module provides support for the ARC processor family's target     */
/*     dependencies which are specific to the arc-elf32 configuration of the  */
/*     ARC gdb.                                                               */
/*                                                                            */
/*                                                                            */
/*  Functionality:                                                            */
/*     This module provides a number of operations:                           */
/*                                                                            */
/*     1) a function which returns the name of a register, given its number   */
/*                                                                            */
/*     2) a function which determines whether a given register should be      */
/*        saved and restored across a function call                           */
/*                                                                            */
/*     3) a function which prints out registers                               */
/*                                                                            */
/*     4) functions which implement the gdb extended commands                 */
/*           arc-aux-read <from> <to>          for displaying aux registers   */
/*           arc-aux-write <regnum> = <value>  for setting an aux register    */
/*                                                                            */
/*                                                                            */
/* Usage:                                                                     */
/*     This module exports a function arc_jtag_init which creates the user    */
/*     commands which use those command-implementing functions; it also       */
/*     stores pointers to the other functions in a data structure so that     */
/*     they may be called from outside this module.                           */
/*                                                                            */
/*     Some of the operations provided by this module are registered with gdb */
/*     during initialization; gdb then calls them via function pointers,      */
/*     rather than by name (this allows gdb to handle multiple target         */
/*     architectures):                                                        */
/*                                                                            */
/*          set_gdbarch_XXX (gdbarch, <function>);                            */
/*                                                                            */
/******************************************************************************/

/* system header files */
#include <string.h>

/* gdb header files */
#include "defs.h"
#include "inferior.h"
#include "gdbcmd.h"
#include "reggroups.h"

/* ARC header files */
#include "arc-jtag-tdep.h"
#include "arc-tdep.h"
#include "arc-jtag.h"


/* -------------------------------------------------------------------------- */
/*                               local data                                   */
/* -------------------------------------------------------------------------- */

#define AUX_READ_COMMAND            "arc-aux-read"
#define AUX_WRITE_COMMAND           "arc-aux-write"
#define WATCH_MEMORY_COMMAND        "arc-watch-range"
#define BREAK_MEMORY_COMMAND        "arc-break-range"
#define FILL_MEMORY_COMMAND         "arc-fill-memory"

#define AUX_READ_COMMAND_USAGE      "Usage: " AUX_READ_COMMAND     " <REG-FROM> [<REG-TO>]\n"
#define AUX_WRITE_COMMAND_USAGE     "Usage: " AUX_WRITE_COMMAND    " <REG> = <VALUE>\n"
#define WATCH_MEMORY_COMMAND_USAGE  "Usage: " WATCH_MEMORY_COMMAND " <START> <LENGTH> [read|write|access]\n"
#define BREAK_MEMORY_COMMAND_USAGE  "Usage: " BREAK_MEMORY_COMMAND " <START> <LENGTH>\n"
#define FILL_MEMORY_COMMAND_USAGE   "Usage: " FILL_MEMORY_COMMAND  " <START> <LENGTH> [<PATTERN>]\n"



/* ARC 700 */
/* brk_s instruction */
static const unsigned char breakpoint_instruction[] = { 0xff, 0x7f };


/* N.B. the array size is specified in the declaration so that the compiler
 *      will warn of "excess elements in array initializer" if there is a
 *      mismatch (but not of too few elements, unfortunately!).
 */
static const char* register_names[ARC_NR_REGS + ARC_NR_PSEUDO_REGS] =
{
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",
    "r7",  "r8",  "r9",  "r10", "r11", "r12", "r13",
    "r14", "r15", "r16", "r17", "r18", "r19", "r20",
    "r21", "r22", "r23", "r24", "r25", "r26",

    "fp",      // r27
    "sp",      // r28
    "ilink1",  // r29
    "ilink2",  // r30
    "blink",   // r31

    /* Extension core registers are 32 .. 59 inclusive. */
    "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39",
    "r40", "r41", "r42", "r43", "r44", "r45", "r46", "r47", "r48", "r49",
    "r50", "r51", "r52", "r53", "r54", "r55", "r56", "r57", "r58", "r59",

    "lp_count",

    /* 61 is reserved, 62 is not a real register. */
    "r61",
    "r62",

    "pcl",

    /* Now the Auxiliary registers. */

    "status",
    "semaphore",
    "lp_start",
    "lp_end",
    "identity",
    "debug",

    "pc",
    "status32",
    "status32_l1",
    "status32_l2",

    "count0",
    "control0",
    "limit0",
    "int_vector_base",
    "aux_macmode",
    "aux_irq_lv12",

    "count1",
    "control1",
    "limit1",
    "aux_irq_lev",
    "aux_irq_hint",
    "eret",
    "erbta",
    "erstatus",
    "ecr",
    "efa",
    "icause1",
    "icause2",
    "aux_ienable",
    "aux_itrigger",
    "xpu",
    "bta",
    "bta_l1",
    "bta_l2",
    "aux_irq_pulse_cancel",
    "aux_irq_pending",

    /* Build Configuration Registers. */
    "bcr_0",
    "dccm_base_build",
    "crc_base_build",
    "bta_link_build",
    "dvbf_build",
    "tel_instr_build",
    "bcr_6",
    "memsubsys",
    "vecbase_ac_build",
    "p_base_address",
    "bcr_a",
    "bcr_b",
    "bcr_c",
    "bcr_d",
    "bcr_e",
    "mmu_build",
    "arcangel_build",
    "bcr_11",
    "d_cache_build",
    "madi_build",
    "dccm_build",
    "timer_build",
    "ap_build",
    "icache_build",
    "iccm_build",
    "dspram_build",
    "mac_build",
    "multiply_build",
    "swap_build",
    "norm_build",
    "minmax_build",
    "barrel_build"
};


static ARC_VariantsInfo debug_processor_information;


/* -------------------------------------------------------------------------- */
/*                               local macros                                 */
/* -------------------------------------------------------------------------- */

#define PRINT(regnum) \
    default_print_registers_info (gdbarch, file, frame, regnum, all)


/* -------------------------------------------------------------------------- */
/*                               local functions                              */
/* -------------------------------------------------------------------------- */

/* Returns 0, 1, or -1:
 *    0 means the register is not in the group.
 *    1 means the register is in the group.
 *   -1 means the tdep has nothing to say about this register and group.
 */
static int
register_reggroup_p (int regnum, struct reggroup *group)
{
    /* These registers don't exist, so they are not in any reggroup.  */
    if ((32 <= regnum && regnum <= 59) || (regnum == 61) || (regnum == 62))
        return 0;

    /* Which regs to save/restore? */
    if ((group == save_reggroup || group == restore_reggroup))
    {
        /* Save/restore:
         *    1. all core regs, except PCL (PCL is not writable)
         *    2. aux regs LP_START .. LP_END (IDENTITY is not writable)
         *    3. aux regs PC_REGNUM .. STATUS32_L2
         *    4. aux regs ERET .. EFA
         */
        return ((0                   <= regnum && regnum  < ARC_PCL_REGNUM)         ||
                (ARC_LP_START_REGNUM <= regnum && regnum <= ARC_LP_END_REGNUM)      ||
                (ARC_PC_REGNUM       <= regnum && regnum <= ARC_STATUS32_L2_REGNUM) ||
                (ARC_ERET_REGNUM     <= regnum && regnum <= ARC_EFA_REGNUM)) ? 1 : 0;
    }

    /* let the caller sort it out! */
    return -1;
}


static void memory_range_command(char*       arg,
                                 int         from_tty,
                                 Boolean     is_watchpoint,
                                 const char* command,
                                 const char* usage)
{
    char                  *length_arg;
    struct expression     *start_expr, *length_expr;
    struct value          *start_val,  *length_val;
    struct cleanup        *old_chain;
    unsigned int           start;
    unsigned int           length;
    enum target_hw_bp_type type;

    if (!arg)
    {
        printf_filtered ("%s", usage);
        return;
    }

    length_arg = strchr(arg, ' ');

    if (!length_arg)
    {
        printf_filtered ("%s : no second argument\n%s", command, usage);
        return;
    }

    /* split up the input string */
    length_arg[0] = (char) 0;
    length_arg++;
    while (*length_arg == ' ') length_arg++;

    if (is_watchpoint)
    {
        char* access_arg = strchr(length_arg, ' ');

        if (access_arg)
        {
            /* split up the input string */
            access_arg[0] = (char) 0;
            access_arg++; 
            while (*access_arg == ' ') access_arg++;

            if (strcmp(access_arg, "read") == 0)
                type = hw_read;
            else if (strcmp(access_arg, "write") == 0)
                type = hw_write;
            else if (strcmp(access_arg, "access") == 0)
                type = hw_access;
            else
            {
                printf_filtered ("%s: invalid type '%s'\n%s", command, access_arg, usage);
                return;
            }
        }
        else
            // write by default
            type = hw_write;
    }
    else
        type = hw_execute;

    /* from address expression */
    start_expr = parse_expression (arg);
    start_val  = evaluate_expression (start_expr);
    start      = *(unsigned int *)(value_contents (start_val));
    old_chain   = make_cleanup (free_current_contents, &start_expr);

    /* length expression */
    length_expr = parse_expression (length_arg);
    length_val  = evaluate_expression (length_expr);
    length      = *(unsigned int *)(value_contents (length_val));
    (void) make_cleanup (free_current_contents, &length_expr);

    if (length <= 0)
    {
        warning ("%s: %s <= 0", command, length_arg);
        do_cleanups (old_chain);
        return;
    }

    DEBUG("try to set %u breakpoint at 0x%08X length %u bytes\n",
          type, start, length);

    watch_range_command(start, length, type, from_tty);

    /* clean up the items that we have put on the cleanup chain (back as far as
     * the old head of the chain
     */
    do_cleanups (old_chain);
}


/* -------------------------------------------------------------------------- */
/*                        local functions called from gdb                     */
/* -------------------------------------------------------------------------- */

static void
arc_jtag_print_registers_info (struct gdbarch    *gdbarch,
                               struct ui_file    *file,
                               struct frame_info *frame,
                               int                regnum,
                               int                all)
{
    if (regnum >= 0)
        PRINT(regnum);
    else
    /* if regnum < 0, print registers */
    {
        int i;

        /* r0 .. r26 */
        for (i = 0; i <= 26; i++) PRINT (i);

        PRINT (ARC_FP_REGNUM      );
        PRINT (ARC_SP_REGNUM      );
        PRINT (ARC_ILINK1_REGNUM  );
        PRINT (ARC_ILINK2_REGNUM  );
        PRINT (ARC_BLINK_REGNUM   );
        PRINT (ARC_LP_COUNT_REGNUM);

        /* now the aux registers */
        if (!all)
        {
            PRINT (ARC_LP_START_REGNUM   );
            PRINT (ARC_LP_END_REGNUM     );
            PRINT (ARC_STATUS32_REGNUM   );
            PRINT (ARC_BTA_REGNUM        );
            PRINT (ARC_EFA_REGNUM        );
            PRINT (ARC_ERET_REGNUM       );
            PRINT (ARC_STATUS32_L1_REGNUM);
            PRINT (ARC_STATUS32_L2_REGNUM);
            PRINT (ARC_ERSTATUS_REGNUM   );
            PRINT (ARC_PC_REGNUM         );
        }
        else
        {
            for (i = ARC_STATUS_REGNUM; i <= ARC_AUX_IRQ_PENDING_REGNUM; i++) PRINT (i);
            for (i = ARC_BCR_1_REGNUM;  i <= ARC_BCR_5_REGNUM;           i++) PRINT (i);
            for (i = ARC_BCR_7_REGNUM;  i <= ARC_BCR_9_REGNUM;           i++) PRINT (i);
            for (i = ARC_BCR_F_REGNUM;  i <= ARC_BCR_10_REGNUM;          i++) PRINT (i);
            for (i = ARC_BCR_12_REGNUM; i <= ARC_BCR_1F_REGNUM;          i++) PRINT (i);
        }
    }
}


/* Command: <command> <from> [ <to> ]
 *
 * Read and display a range of aux registers.  Some of the aux registers
 * (pc, debug, etc.) are part of the register set, but this is a more
 * general interface.
 *
 * We should eventually change this to use the ui_out stuff rather than
 * printf_filtered.
 */
static void
arc_jtag_aux_read_command (char *arg, int from_tty)
{
    char                 *arg2;
    struct expression    *expr;
    struct value         *val;
    struct cleanup       *old_chain;
    ARC_RegisterContents *buf;
    int                   first_aux_reg_no, last_aux_reg_no, num_regs, num_registers_read;

    if (!arg)
    {
        printf_filtered (AUX_READ_COMMAND_USAGE);
        return;
    }

    /* strip leading spaces */
    while (*arg == ' ')
        arg++;

    /* This assumes that the first arg cannot have spaces.  (The disas command
       also seems to work this way.)  */
    arg2 = strchr (arg, ' ');

    /* are there two arguments? */
    if (arg2)
    {
        /* split the input string up */
        arg2[0] = (char) 0;
        arg2++;
    }

    /* first arg */
    expr = parse_expression (arg);
    val  = evaluate_expression (expr);
    old_chain = make_cleanup (free_current_contents, &expr);

    first_aux_reg_no = *(int*)(value_contents (val));

    if (first_aux_reg_no < 0)
    {
        warning (AUX_READ_COMMAND ": %s < 0", arg);
        do_cleanups (old_chain);
        return;
    }

    /* so, how many regs do we want? */
    if (arg2)
    {
        struct expression *expr2 = parse_expression (arg2);
        struct value      *val2  = evaluate_expression (expr2);

        (void) make_cleanup (free_current_contents, &expr);

        last_aux_reg_no = *(int *)(value_contents (val2));

        if (last_aux_reg_no < 0)
        {
            warning (AUX_READ_COMMAND ": %s < 0", arg2);
            do_cleanups (old_chain);
            return;
        }

        if (last_aux_reg_no < first_aux_reg_no)
        {
            warning (AUX_READ_COMMAND ": %s < %s, showing one register", arg2, arg);
            last_aux_reg_no = first_aux_reg_no;
        }
    }
    else
        last_aux_reg_no = first_aux_reg_no;

    DEBUG("try to read aux regs %d .. %d\n", first_aux_reg_no, last_aux_reg_no);

    num_regs = last_aux_reg_no - first_aux_reg_no + 1;

    buf = xcalloc ((size_t) num_regs, sizeof(ARC_RegisterContents));
    (void) make_cleanup (free_current_contents, &buf);

    /* Go get 'em!  */
    num_registers_read = (int) target_read_aux_reg (buf,
                                                    (ULONGEST) first_aux_reg_no,
                                                    (LONGEST)  num_regs);

    if (num_registers_read <= 0)
        warning (AUX_READ_COMMAND ": could not read any registers.");
    else
    {
        int count = 0;
        int i;

        gdb_assert (num_registers_read <= num_regs);

        if (num_registers_read < num_regs)
        {
            warning (AUX_READ_COMMAND ": could read only %d registers",
                     num_registers_read);
            last_aux_reg_no = first_aux_reg_no + num_registers_read - 1;
        }

        /* Show them, 4 per line. */
        for (i = first_aux_reg_no; i <= last_aux_reg_no; i++)
        {
            /* include the register number at the beginning of each line;
             * for each line except the first, output a newline at the start
             */
            if (count % 4 == 0)
                printf_filtered("%s%08x: ", (count ? "\n" : ""), i);

            printf_filtered ("%08x ", buf[count]);
            count++;
        } 

        printf_filtered ("\n");
    }

    do_cleanups (old_chain);
}


/* arc-aux-write <regnum> = <value>
   Write VALUE to aux register REGNUM. */
static void
arc_jtag_aux_write_command (char *arg, int from_tty)
{
    char                *value_arg;
    struct expression   *regnum_expr, *value_expr;
    struct value        *regnum_val,  *value_val;
    struct cleanup      *old_chain;
    int                  regnum;
    ARC_RegisterContents value;
    LONGEST              num_registers_written;

    if (!arg)
    {
        printf_filtered (AUX_WRITE_COMMAND_USAGE);
        return;
    }

    value_arg = strchr(arg, '=');

    if (!value_arg)
    {
        printf_filtered (AUX_WRITE_COMMAND ": no second argument\n" AUX_WRITE_COMMAND_USAGE);
        return;
    }

    /* split up the input string */
    value_arg[0] = (char) 0;
    value_arg++;

    /* Regnum expression */
    regnum_expr = parse_expression (arg);
    regnum_val  = evaluate_expression (regnum_expr);
    regnum      = *(int *)(value_contents (regnum_val));
    old_chain   = make_cleanup (free_current_contents, &regnum_expr);

    /* Value expression */
    value_expr = parse_expression (value_arg);
    value_val  = evaluate_expression (value_expr);
    value      = *(ARC_RegisterContents*)(value_contents (value_val));
    (void) make_cleanup (free_current_contents, &value_expr);

    if (regnum < 0)
    {
        warning (AUX_WRITE_COMMAND ": %s < 0", arg);
        do_cleanups (old_chain);
        return;
    }

    DEBUG("try to write aux reg %d = 0x%08X\n", regnum, value);

    /* Write it. */
    num_registers_written = target_write_aux_reg ((gdb_byte*) &value,
                                                  (ULONGEST) regnum,
                                                  1);

    if (num_registers_written != 1)
        warning (AUX_WRITE_COMMAND ": could not write to register %d", regnum);

    /* clean up the items that we have put on the cleanup chain (back as far as
     * the old head of the chain
     */
    do_cleanups (old_chain);
}


/* arc-watch-range <start> <length>
   Set hardware breakpoint at address START covering LENGTH bytes. */
static void
arc_jtag_break_memory_command(char *arg, int from_tty)
{
    memory_range_command(arg, from_tty, FALSE, BREAK_MEMORY_COMMAND, BREAK_MEMORY_COMMAND_USAGE);
}


/* arc-watch-range <start> <length> [read|write|access]
   Set hardware watchpoint at address START covering LENGTH bytes. */
static void
arc_jtag_watch_memory_command (char *arg, int from_tty)
{
    memory_range_command(arg, from_tty, TRUE, WATCH_MEMORY_COMMAND, WATCH_MEMORY_COMMAND_USAGE);
}


/* arc-fill-memory <start> <length> [<pattern>]
   Write repeated copies of PATTERN at address START covering LENGTH bytes. */
static void
arc_jtag_fill_memory_command (char *arg, int from_tty)
{
    char              *length_arg;
    char              *pattern_arg;
    struct expression *start_expr, *length_expr, *pattern_expr;
    struct value      *start_val,  *length_val,  *pattern_val;
    struct cleanup    *old_chain;
    ARC_Address       start;
    unsigned int      length;
    ARC_Word          pattern;
    unsigned int      written;

    if (!arg)
    {
        printf_filtered ("%s", FILL_MEMORY_COMMAND_USAGE);
        return;
    }

    length_arg = strchr(arg, ' ');

    if (!length_arg)
    {
        printf_filtered (FILL_MEMORY_COMMAND " : no second argument\n" FILL_MEMORY_COMMAND_USAGE);
        return;
    }

    /* split up the input string */
    length_arg[0] = (char) 0;
    length_arg++;
    while (*length_arg == ' ') length_arg++;

    pattern_arg = strchr(length_arg, ' ');
    if (pattern_arg)
    {
        /* split up the input string */
        pattern_arg[0] = (char) 0;
        pattern_arg++;
    }

    /* from address expression */
    start_expr = parse_expression (arg);
    start_val  = evaluate_expression (start_expr);
    start      = *(ARC_Address *)(value_contents (start_val));
    old_chain   = make_cleanup (free_current_contents, &start_expr);

    /* length expression */
    length_expr = parse_expression (length_arg);
    length_val  = evaluate_expression (length_expr);
    length      = *(unsigned int *)(value_contents (length_val));
    (void) make_cleanup (free_current_contents, &length_expr);

    if (length <= 0)
    {
        warning (FILL_MEMORY_COMMAND ": %s <= 0", length_arg);
        do_cleanups (old_chain);
        return;
    }

    if (pattern_arg)
    {
        /* from pattern expression */
        pattern_expr = parse_expression (pattern_arg);
        pattern_val  = evaluate_expression (pattern_expr);
        pattern      = *(ARC_Word *)(value_contents (pattern_val));
        (void) make_cleanup (free_current_contents, &pattern_expr);
    }
    else
        pattern = 0;


    written = arc_jtag_ops.jtag_memory_write_pattern(start, pattern, length);

    if (written != length)
        warning (FILL_MEMORY_COMMAND ": only %u bytes written to target memory", written);

    /* clean up the items that we have put on the cleanup chain (back as far as
     * the old head of the chain
     */
    do_cleanups (old_chain);
}




/* -------------------------------------------------------------------------- */
/*                               externally visible functions                 */
/* -------------------------------------------------------------------------- */

struct gdbarch *
arc_jtag_init (struct gdbarch *gdbarch)
{
    struct gdbarch_tdep *tdep = gdbarch_tdep (gdbarch);

    /* Fill in target-dependent info in ARC-private structure. */

    tdep->is_sigtramp             = NULL;
    tdep->sigcontext_addr         = NULL;
    tdep->sc_reg_offset           = NULL;
    tdep->sc_num_regs             = 0;
    tdep->pc_regnum_in_sigcontext = 0;

    tdep->breakpoint_instruction = breakpoint_instruction;
    tdep->breakpoint_size        = (unsigned int) sizeof(breakpoint_instruction);

    tdep->register_reggroup_p = register_reggroup_p;
    tdep->register_names      = register_names;
    tdep->num_register_names  = ELEMENTS_IN_ARRAY(register_names);

    tdep->lowest_pc              = 0;
    tdep->processor_variant_info = &debug_processor_information;

    /* Pass target-dependent info to gdb. */

    set_gdbarch_pc_regnum            (gdbarch, ARC_PC_REGNUM);
    set_gdbarch_print_registers_info (gdbarch, arc_jtag_print_registers_info);

    /* Register auxiliary register commands. */

    (void) add_cmd (AUX_READ_COMMAND,
                    class_vars,
                    arc_jtag_aux_read_command,
                    "Read and show a range of auxiliary registers.\n"
                    AUX_READ_COMMAND_USAGE
                    "REG-FROM and REG-TO can be any expressions that evaluate to integers.\n"
                    "If REG-TO is not specified, one register is displayed.",
                    &cmdlist);

    (void) add_cmd (AUX_WRITE_COMMAND,
                    class_vars,
                    arc_jtag_aux_write_command,
                    "Write to an auxiliary register.\n"
                    AUX_WRITE_COMMAND_USAGE
                    "REG and VALUE can be any expressions that evaluate to integers.",
                    &cmdlist);

    (void) add_cmd (BREAK_MEMORY_COMMAND,
                    class_breakpoint,
                    arc_jtag_break_memory_command,
                    "Set a breakpoint on a memory address range.\n"
                    BREAK_MEMORY_COMMAND_USAGE
                    "START and LENGTH can be any expressions that evaluate to integers.",
                    &cmdlist);

    (void) add_cmd (WATCH_MEMORY_COMMAND,
                    class_breakpoint,
                    arc_jtag_watch_memory_command,
                    "Set a watchpoint on a memory address range.\n"
                    WATCH_MEMORY_COMMAND_USAGE
                    "START and LENGTH can be any expressions that evaluate to integers.",
                    &cmdlist);

    (void) add_cmd (FILL_MEMORY_COMMAND,
                    class_obscure,
                    arc_jtag_fill_memory_command,
                    "Fill a memory address range with a repeated pattern.\n"
                    FILL_MEMORY_COMMAND_USAGE
                    "START, LENGTH and PATTERN can be any expressions that evaluate to integers.\n"
                    "If PATTERN is omitted, it defaults to 0.",
                    &cmdlist);

    return gdbarch;
}

/******************************************************************************/
