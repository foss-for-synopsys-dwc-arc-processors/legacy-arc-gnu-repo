/******************************************************************************/
/*                                                                            */
/* Outline:                                                                   */
/*     This header file defines register numbers for the arc-linux-uclibc     */
/*     configuration of the ARC gdb.                                          */
/*                                                                            */
/******************************************************************************/

#ifndef ARC_TM_LINUX_H
#define ARC_TM_LINUX_H


#define CONFIG_OSABI        GDB_OSABI_LINUX

/* Do nothing.  */
#define CONFIG_INIT_TDEP    {}


enum arc700_linux_regnums
{
    /* Regnums 0 .. 26 are R0 .. R26 */
    ARC_BTA_REGNUM      = 27,
    ARC_LP_START_REGNUM = 28,
    ARC_LP_END_REGNUM   = 29,
    ARC_LP_COUNT_REGNUM = 30,
    ARC_STATUS32_REGNUM = 31,
    ARC_BLINK_REGNUM    = 32,
    ARC_FP_REGNUM       = 33,
    ARC_SP_REGNUM       = 34,
    ARC_EFA_REGNUM      = 35,
    ARC_RET_REGNUM      = 36,
    ARC_ORIG_R8_REGNUM  = 37,
    ARC_STOP_PC_REGNUM  = 38,

   /* end marker: this is not a register, but its integer value gives the number
    * of registers
    */
    ARC_NR_REGS
};


/* Pseudo-registers.
 *
 * N.B. NUM_REGS is not a constant - rather, it is a call to gdbarch_num_regs!
 *
 * FIXME: given that gdbarch_num_regs simply returns the value passed to function
 *        set_gdbarch_num_regs, and that function is called with parameter
 *        ARC_NR_REGS, it should be possible simply to declare these as an enum
 *        type with values starting at ARC_NR_REGS (this would be rather more
 *        efficient as it would not involve a function call each time one of
 *        thses values is used!).
 */
#define ARC_ILINK1_REGNUM       (NUM_REGS)
#define ARC_ILINK2_REGNUM       (NUM_REGS + 1)
#define ARC_ERET_REGNUM         (NUM_REGS + 2)
#define ARC_STATUS32_L1_REGNUM  (NUM_REGS + 3)
#define ARC_STATUS32_L2_REGNUM  (NUM_REGS + 4)
#define ARC_ERSTATUS_REGNUM     (NUM_REGS + 5)

#define ARC_NR_PSEUDO_REGS      6


#endif /* ARC_TM_LINUX_H */
/******************************************************************************/
