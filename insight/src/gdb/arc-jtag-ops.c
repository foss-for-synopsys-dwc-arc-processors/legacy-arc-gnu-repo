/* Target dependent code for ARC700, for GDB, the GNU debugger.

   Copyright 2008 Free Software Foundation, Inc.

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
/*     This module implements debug access to an ARC processor via its JTAG   */
/*     interface.                                                             */
/*                                                                            */
/*     See                                                                    */
/*           ARCompact Instruction Set Architecture                           */
/*              Programmer's Reference    (5115-018)                          */
/*                                                                            */
/*           for a description of ARC processor architecture (in particular   */
/*           the auxiliary registers and the halting procedure);              */
/*                                                                            */
/*           ARC 700 External Interfaces                                      */
/*              Reference                 (5117-013)                          */
/*                                                                            */
/*           for a description of the JTAG interface (in particular the Test  */
/*           Access Port Controller (TAPC) state machine).                    */
/*                                                                            */
/*     The JTAG interface is accessed by three parallel ports: control, data  */
/*     and status.  Data is read from or written to these ports one byte at a */
/*     at a time, using a GPIO (General Purpose Input/Output) driver.         */
/*                                                                            */
/*     The TDI and TMS signals are written to the data port.                  */
/*     The TCK signal is written to the control port.                         */
/*     The TDO signal is read from the status port.                           */
/*                                                                            */
/*                                                                            */
/*     The read/write chunk functions may be required to transfer arbitrary   */
/*     amounts of data to/from arbitrary addresses in memory; however, the    */
/*     JTAG read/write operations can only transfer one word of data to/from  */
/*     a word-aligned address.                                                */
/*                                                                            */
/*     This gives the general case:                                           */
/*                                                                            */
/*                           word boundaries                                  */
/*                                  |                                         */
/*           -------------------------------------------------                */
/*          |         |         |         |         |         |               */
/*      -----------------------------------------------------------           */
/*          | g g L L | W W W W | W W W W | W W W W | T T e e |               */
/*      -----------------------------------------------------------           */
/*                ^                                                           */
/*                |-------------------------------------|                     */
/*                |                len                                        */
/*                addr                                                        */
/*                                                                            */
/*       where addr is the base address of the data to be transferred         */
/*             len  is the number of bytes of data to be transferred          */
/*             W denotes a byte that can be transfered in a whole word        */
/*             L denotes a leading byte                                       */
/*             T denotes a trailing byte                                      */
/*             g denotes a gap byte                                           */
/*             e denotes a end gap byte                                       */
/*                                                                            */
/*     There may 0 .. BYTES_IN_WORD - 1 leading bytes, 0 or more whole words, */
/*     and 0 .. BYTES_IN_WORD - 1 trailing bytes. If the given address is     */
/*     word-aligned, there is no gap and hence no leading bytes.              */
/*                                                                            */
/*     There is also a pathological case:                                     */
/*                                                                            */
/*                           word boundaries                                  */
/*                                  |                                         */
/*                               ---------                                    */
/*                              |         |                                   */
/*      -----------------------------------------------------------           */
/*                              | g B B e |                                   */
/*      -----------------------------------------------------------           */
/*                                  ^                                         */
/*                                  |-|                                       */
/*                                  | len                                     */
/*                                  addr                                      */
/*                                                                            */
/*     where 1 .. BYTES_IN_WORD - 2 bytes of data in the middle of a word     */
/*     must be transfered.                                                    */
/*                                                                            */
/*     In a write operation, it is necessary to preserve the contents of the  */
/*     gap and end gap bytes, if any - this is done by first reading the word */
/*     which contains those bytes, constructing a new word which contains     */
/*     those bytes and the new bytes, and writing the new value back to that  */
/*     location.                                                              */
/*                                                                            */
/******************************************************************************/

/* for internal debugging */
//#define STATE_MACHINE_DEBUG 1
//#define STANDALONE_TEST     1


/* system header files */
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>

/* gdb header files */
#include "defs.h"
#include "arc-gpio.h"

/* ARC header files */
#include "arc-jtag-ops.h"
#include "arc-support.h"


/* -------------------------------------------------------------------------- */
/*                               local types                                  */
/* -------------------------------------------------------------------------- */

/* sizes of quantities */
#define BITS_IN_COMMAND_CODE     4
#define BITS_IN_REGISTER_CODE    4


typedef unsigned int  Bit;    /* only LSB of word is used */
typedef unsigned char Byte;
typedef unsigned int  JTAG_RegisterContents;


typedef enum
{
    MSB_FIRST,
    MSB_LAST
} Order;


typedef enum
{
    Memory,
    Register
} JTAG_TransactionType;


typedef enum
{
    STALLED,
    FAILURE,
    READY,
    NOT_READY
} JTAG_TransactionStatus;


/* only these JTAG registers are currently used */
typedef enum
{
    JTAG_STATUS_REGISTER              = 0x8,
    JTAG_TRANSACTION_COMMAND_REGISTER = 0x9,
    JTAG_ADDRESS_REGISTER             = 0xA,
    JTAG_DATA_REGISTER                = 0xB,
} JTAG_Register;


typedef enum
{
    DATA_PORT    = 0,   
    STATUS_PORT  = 1,
    CONTROL_PORT = 2
} ParallelPort;


#ifdef STATE_MACHINE_DEBUG
typedef enum
{
    UNDEFINED,
    TEST_LOGIC_RESET,
    RUN_TEST_IDLE,
    SELECT_DR_SCAN,
    CAPTURE_DR,
    SHIFT_DR,
    EXIT1_DR,
    PAUSE_DR,
    EXIT2_DR,
    UPDATE_DR,
    SELECT_IR_SCAN,
    CAPTURE_IR,
    SHIFT_IR,
    EXIT1_IR,
    PAUSE_IR,
    EXIT2_IR,
    UPDATE_IR,
    NUMBER_OF_STATES   // an end-marker, not a state
} JTAG_ControllerState;
#endif


typedef struct
{
    unsigned int leading_bytes;
    unsigned int trailing_bytes;
    unsigned int words;
} Layout;


typedef union
{
    ARC_Word word;
    ARC_Byte byte[BYTES_IN_WORD];
} WordBytes;


/* -------------------------------------------------------------------------- */
/*                               local data                                   */
/* -------------------------------------------------------------------------- */

/* -------------------------------------- */
/*         For ARC JTAG Cable             */
/*                                        */
/* Pin no.  Signal         Port       Bit */
/*                                        */
/* - 	    TRST 	                  */
/* 8 	    TMS 	   Data       6   */
/* 1 	    TCK 	   Control    0   */
/* 9 	    TDI 	   Data       7   */
/* 13 	    TDO 	   Status     4   */
/* -------------------------------------- */

/* bit masks for signals written to parallel ports */

#define JTAG_TRST 0          /* not there                    */
#define JTAG_TMS  (1 << 6)   /* on Data port                 */
#define JTAG_TCK  (1 << 0)   /* on Control port (driven low) */
#define JTAG_TDI  (1 << 7)   /* on Data port                 */
#define JTAG_TDO  (1 << 4)   /* on Status port               */


/* parallel port I/O addresses (LPT1) */
#define PORT_BASE_ADDRESS     0x378
#define DATA_PORT_ADDRESS     (PORT_BASE_ADDRESS + DATA_PORT)
#define STATUS_PORT_ADDRESS   (PORT_BASE_ADDRESS + STATUS_PORT)
#define CONTROL_PORT_ADDRESS  (PORT_BASE_ADDRESS + CONTROL_PORT)


/* commands which can be written to the JTAG Transaction Command Register */
#define WRITE_MEMORY_LOCATION   0x0
#define WRITE_CORE_REGISTER     0x1
#define WRITE_AUX_REGISTER      0x2
#define NOP                     0x3
#define READ_MEMORY_LOCATION    0x4
#define READ_CORE_REGISTER      0x5
#define READ_AUX_REGISTER       0x6


/* these accumulate the bit masks to be written to the data and control ports */
static Byte data_port_value, control_port_value;

/* basic I/O */
static int               gpio_driver;
static struct GPIO_ioctl jtag_ioctl;
static Boolean           port_error;

static ARC_ProcessorVersion ARC_processor;

/* variables for tracking the contents of the JTAG Address and Transaction
 * Command registers
 */
static JTAG_RegisterContents address_register_contents;
static JTAG_RegisterContents command_register_contents;
static Boolean               address_register_contents_known;
static Boolean               command_register_contents_known;


#ifdef STATE_MACHINE_DEBUG
/* this table encodes all possible transitions of the JTAG Test Access Port
 * (TAP) Controller State Machine: for each state, the transition to one of two
 * possible next states is determined by whether a 0 bit or a 1 bit is written
 * as the JTAG TMS interface signal
 */
static const JTAG_ControllerState transitions[NUMBER_OF_STATES][2] =
{
/*                              0              1           */
/*  UNDEFINED         */  { UNDEFINED,     UNDEFINED        },
/*  TEST_LOGIC_RESET  */  { RUN_TEST_IDLE, TEST_LOGIC_RESET },
/*  RUN_TEST_IDLE     */  { RUN_TEST_IDLE, SELECT_DR_SCAN   },
/*  SELECT_DR_SCAN    */  { CAPTURE_DR,    SELECT_IR_SCAN   },
/*  CAPTURE_DR        */  { SHIFT_DR,      EXIT1_DR         },
/*  SHIFT_DR          */  { SHIFT_DR,      EXIT1_DR         },
/*  EXIT1_DR          */  { PAUSE_DR,      UPDATE_DR        },
/*  PAUSE_DR          */  { PAUSE_DR,      EXIT2_DR         },
/*  EXIT2_DR          */  { SHIFT_DR,      UPDATE_DR        },
/*  UPDATE_DR         */  { RUN_TEST_IDLE, SELECT_DR_SCAN   },
/*  SELECT_IR_SCAN    */  { CAPTURE_IR,    TEST_LOGIC_RESET },
/*  CAPTURE_IR        */  { SHIFT_IR,      EXIT1_IR         },
/*  SHIFT_IR          */  { SHIFT_IR,      EXIT1_IR         },
/*  EXIT1_IR          */  { PAUSE_IR,      UPDATE_IR        },
/*  PAUSE_IR          */  { PAUSE_IR,      EXIT2_IR         },
/*  EXIT2_IR          */  { SHIFT_IR,      UPDATE_IR        },
/*  UPDATE_IR         */  { RUN_TEST_IDLE, SELECT_DR_SCAN   },
};

/* the current state of the TAP Controller State Machine */
static JTAG_ControllerState current_state = UNDEFINED;
#endif


/* -------------------------------------------------------------------------- */
/*                               externally visible data                      */
/* -------------------------------------------------------------------------- */

JTAG_Operations arc_jtag_ops;


/* -------------------------------------------------------------------------- */
/*                               local macros                                 */
/* -------------------------------------------------------------------------- */

#ifdef ARC_DEBUG
#undef DEBUG
#define DEBUG(...) \
    if (arc_jtag_ops.jtag_state_machine_debug) printf(__VA_ARGS__)
#else
#endif


/* N.B. it must be possible to build this module without the rest of gdb so
 *      that it can be exercised by a stand-alone test driver
 */
#ifdef STANDALONE_TEST
#define error(...)            printf(__VA_ARGS__)
#define warning(...)          printf(__VA_ARGS__)
#define printf_filtered(...)  printf(__VA_ARGS__)
#endif

#ifdef STATE_MACHINE_DEBUG
#define SET_STATE(s)                  set_state(s)
#define NEXT_STATE(b)                 next_state(b)
#define CHANGE_STATE(x, s)            change_state(x, s) 
#define STATE_IS(s)                   assert(current_state == s)
#define STATE_IS_EITHER(s1, s2)       assert(current_state == s1 || \
                                             current_state == s2)
#define STATE_IS_ONE_OF(s1, s2, s3)   assert(current_state == s1 || \
                                             current_state == s2 || \
                                             current_state == s3)
#else
#define SET_STATE(s)
#define NEXT_STATE(b)
#define CHANGE_STATE(x, s)            tapc_TMS(x)
#define STATE_IS(s)
#define STATE_IS_EITHER(s1, s2)
#define STATE_IS_ONE_OF(s1, s2, s3)
#endif

#define IS_WORD_ALIGNED(addr)         ((addr) % BYTES_IN_WORD == 0)
#define BYTE(val)                     (Byte) ((val) & 0xFF)


/* -------------------------------------------------------------------------- */
/*                               local functions                              */
/* -------------------------------------------------------------------------- */

static void tapc_TMS(Bit x);


/* Sanity check to give error if JTAG i/f is not opened at all. */

static void
check_and_raise_error_if_JTAG_closed(void)
{
    if (arc_jtag_ops.jtag_status == JTAG_CLOSED)
    {
        error("JTAG connection is closed. "
              "Use command 'target " ARC_TARGET_NAME "' first.\n");
    }
}


#ifdef ARC_DEBUG
static const char* name(JTAG_Register r)
{
    switch (r)
    {
        case JTAG_STATUS_REGISTER              : return "Status";
        case JTAG_TRANSACTION_COMMAND_REGISTER : return "Transaction Command";
        case JTAG_ADDRESS_REGISTER             : return "Address";
        case JTAG_DATA_REGISTER                : return "Data";
        default: abort();
    }
}


#ifdef STATE_MACHINE_DEBUG
static const char* JTAG_ControllerState_Image(JTAG_ControllerState state)
{
    switch (state)
    {
        case UNDEFINED       : return "<undefined>     ";
        case TEST_LOGIC_RESET: return "Test-Logic-Reset";
        case RUN_TEST_IDLE   : return "Run-Test/Idle   ";
        case SELECT_DR_SCAN  : return "Select-DR-Scan  ";
        case CAPTURE_DR      : return "Capture-DR      ";
        case SHIFT_DR        : return "Shift-DR        ";
        case EXIT1_DR        : return "Exit1-DR        ";
        case PAUSE_DR        : return "Pause-DR        ";
        case EXIT2_DR        : return "Exit2-DR        ";
        case UPDATE_DR       : return "Update-DR       ";
        case SELECT_IR_SCAN  : return "Select-IR-Scan  ";
        case CAPTURE_IR      : return "Capture-IR      ";
        case SHIFT_IR        : return "Shift-IR        ";
        case EXIT1_IR        : return "Exit1-IR        ";
        case PAUSE_IR        : return "Pause-IR        ";
        case EXIT2_IR        : return "Exit2-IR        ";
        case UPDATE_IR       : return "Update-IR       ";
        default              : return "<invalid>       ";
    }
}
#endif
#endif


/* -------------------------------------------------------------------------- */
/* 1)                        primitive I/O functions                          */
/* -------------------------------------------------------------------------- */

/* See the comment in gdb file gpio.h
 *
 * For our purposes, the IOCTL input length is 2, and the input buffer contains
 * either
 *
 *     [0 | 1 | 2, <value>]              -- write <value> to port base + 0 | 1 | 2
 *
 * or
 *
 *     [0x80 | 0x81 | 0x82, <ignored> ]  -- read byte from port base + 0 | 1 | 2
 *                                          and place it in the output buffer
 *
 *
 * N.B. "input" is input TO the port, and "output" is output FROM the port.
 */

/* initializations for GPIO interface */
static int gpio_setup(void)
{
    static char input_buffer [2];
    static char output_buffer[2];

    gpio_driver = open("//dev//gpio", O_RDWR);

    if (gpio_driver == -1)
        return errno;

    if (ioctl(gpio_driver, (unsigned long) GPIO_IOC_HARDRESET) ||
        ioctl(gpio_driver, (unsigned long) GPIO_IOC_SET_PORT_BASE, PORT_BASE_ADDRESS))
        return errno;

    /* set up buffers to hold data read from / written to ports */
    jtag_ioctl.inbuf  = input_buffer;
    jtag_ioctl.outbuf = output_buffer;

    return 0;
}


/* write a byte of data to the given port */
static void gpio_write(ParallelPort port, Byte data)
{
    jtag_ioctl.inlen    = 2;
    jtag_ioctl.inbuf[0] = (char) port;
    jtag_ioctl.inbuf[1] = (char) data;

    if (ioctl(gpio_driver, (unsigned long) GPIO_IOC_DO_IO, &jtag_ioctl))
        error("Failure writing to port %d: %s\n", port, strerror(errno));

    /* if no data has been consumed by the port */
    if (jtag_ioctl.inlen == 0)
        port_error = TRUE;

//  DEBUG("jtag_ioctl.inlen: %d\n", jtag_ioctl.inlen);
}


/* read a byte of data from the given port */
static Byte gpio_read(ParallelPort port)
{
    jtag_ioctl.inlen    = 2;
    jtag_ioctl.inbuf[0] = (char) (port + 0x80);
//  jtag_ioctl.inbuf[1] is ignored

    /* N.B. outlen must be set! */
    jtag_ioctl.outlen    = 1;
    jtag_ioctl.outbuf[0] = (char) 0; // in case the read fails

    if (ioctl(gpio_driver, (unsigned long) GPIO_IOC_DO_IO, &jtag_ioctl))
        error("Failure reading from port %d: %s\n", port, strerror(errno));

    /* if no data has been provided by the port */
    if (jtag_ioctl.outlen == 0)
        port_error = TRUE;

//  DEBUG("jtag_ioctl.outlen: %d\n", jtag_ioctl.outlen);

    return (Byte) jtag_ioctl.outbuf[0];
}


/* -------------------------------------------------------------------------- */
/* 2)            helper functions for setting TMS / TCK / TDI                 */
/* -------------------------------------------------------------------------- */

/* These functions set the accumulated values to be written out to the ports.
 * The final values are written only by doing the pulse, e.g. if TDI and TMS
 * are set/unset by subsequent calls, the last GPIO write operation performed
 * by those calls before the pulse writes the accumulated bit mask value to
 * the port (overwriting the values written by the preceding calls), and it
 * is this bit mask that is significant when the JTAG is clocked.
 */

static void
tapc_set_TMS(Bit x)
{
    if (x)
        data_port_value |=  JTAG_TMS;
    else
        data_port_value &= ~JTAG_TMS;

    gpio_write(DATA_PORT, data_port_value);
}


static void
tapc_set_TDI(Bit x)
{
    if (x)
        data_port_value |=  JTAG_TDI;
    else
        data_port_value &= ~JTAG_TDI;

    gpio_write(DATA_PORT, data_port_value);
}


static void
tapc_set_TCK(Bit x)
{
    /* The clock is active low. */
    if (x)
        control_port_value &= ~JTAG_TCK;
    else
        control_port_value |=  JTAG_TCK;

    gpio_write(CONTROL_PORT, control_port_value);
}


/* -------------------------------------------------------------------------- */
/* 3)                   JTAG state machine handlers                           */
/* -------------------------------------------------------------------------- */

#ifdef STATE_MACHINE_DEBUG
static void set_state(JTAG_ControllerState new_state)
{
    DEBUG("TAPC state: %s ====> %s\n",
           JTAG_ControllerState_Image(current_state),
           JTAG_ControllerState_Image(new_state));

    current_state = new_state;

    assert(current_state != UNDEFINED);
}


static void next_state(Bit x)
{
    current_state = (transitions[current_state][x]);
}


static void change_state(Bit x, JTAG_ControllerState new_state)
{
    tapc_TMS(x);

    assert(current_state == new_state);
}
#endif


/* Clock the JTAG on the ARC platform. */
static void
tapc_pulse(void)
{
    /* TCK control bit is active low */
    assert((control_port_value & JTAG_TCK) != (Byte) 0);  // clock should be zero on entry

    gpio_write(CONTROL_PORT, control_port_value & ~JTAG_TCK);
    gpio_write(CONTROL_PORT, control_port_value);
}


/* Reset the TAP Controller on the JTAG. */
static void
tapc_reset(void)
{
    /* from any state, this many TCK pulses should get the controller into state
     * Test-Logic-Reset
     */
    tapc_set_TMS(1);
    tapc_set_TCK(0);  /* want rising edge */
    tapc_pulse();
    tapc_pulse();
    tapc_pulse();
    tapc_pulse();
    tapc_pulse();
    tapc_pulse();

    SET_STATE(TEST_LOGIC_RESET);

    DEBUG("TAPC has been reset\n");

    /* the reset has re-initialized all the JTAG registers */
    address_register_contents_known = FALSE;
    command_register_contents_known = FALSE;

    CHANGE_STATE(0, RUN_TEST_IDLE);
}


/* Set the TMS to the value of the bit and clock the JTAG.
 *
 * This will cause the TAP Controller State Machine to move to another state.
 */
static void
tapc_TMS(Bit x)
{
    tapc_set_TMS(x);
    tapc_pulse();
    NEXT_STATE(x);
}


/* Read a bit from the TDO of the JTAG. */
static Bit
tapc_readTDO(void)
{
    /* read from the status port */
    return ((gpio_read(STATUS_PORT) & JTAG_TDO) != (Byte) 0) ? (Bit) 1
                                                             : (Bit) 0;
}


/* -------------------------------------------------------------------------- */
/* 4)  interface functions that use the JTAG state machine handler functions  */
/* -------------------------------------------------------------------------- */

/* Shift one bit out on the JTAG TDO and one bit in on the JTAG TDI. */
static Bit
tapc_shift_bit(Bit out)
{
    Bit in = tapc_readTDO();

    tapc_set_TDI(out);
    tapc_pulse();

//  DEBUG("%u (out) >>> %u (in)\n", out, in);

    return in;
}


/* Shift N bits from to_write into TDI, and out from TDO into to_read.
 *
 * If order == MSB_LAST, shift LSB first, starting from to_write[0], to_write[1],
 * etc.
 *
 * If order == MSB_FIRST, shift to_write[N-1] MSB first, then to_write[N-2] etc.
 *
 * Must be called in Shift DR/IR state.
 * Leaves in Exit1-DR/IR state.
 */
static void
tapc_shift_N_bits(unsigned int n,
                  Byte*        to_write,
                  Byte*        to_read,
                  Order        order)
{
    unsigned int nbytes = (n - 1) / BITS_IN_BYTE + 1;
    unsigned int nbits  = BITS_IN_BYTE;
    unsigned int i, j;

    ENTERARGS("shift %u bits", n);

    STATE_IS_EITHER(SHIFT_DR, SHIFT_IR);

    for (i = 0; i < nbytes; i++)
    {
        Boolean is_last_byte = (i == nbytes - 1);
        Byte    inbyte       = (Byte) 0;
        Byte    outbyte;

        if (order == MSB_FIRST)
            outbyte = to_write[nbytes - 1 - i];
        else
            outbyte = to_write[i];

        if (is_last_byte)
        {
            /* how many significant bits in this byte? */
            nbits = ((n - 1) % BITS_IN_BYTE) + 1;
        }

        /* assert (nbits <= BITS_IN_BYTE); */

        for (j = 0; j < nbits; j++)
        {
            Bit outbit, inbit;

//          DEBUG("byte %u, bit %u\n", i, j);

            /* get the next bit to be output from the current byte */
            if (order == MSB_FIRST)
            {
                /* get MSB from byte */
                outbit = (Bit) ((outbyte >> BITS_IN_BYTE) & 1);
                outbyte <<= 1;
            }
            else
            {
                /* get LSB from byte */
                outbit = (Bit) (outbyte & 1);
                outbyte >>= 1;
            }

            /* the last bit of the last byte */
            if (is_last_byte && (j == nbits - 1))
            {
                /* change to Exit1-DR/IR state before the last bit is shifted:
                 * this is necessary because the TAP Controller performs the
                 * last sample of TDI when exiting the Shift-DR/IR state
                 */
                tapc_set_TMS(1);
                NEXT_STATE(1);
            }

            /* shift one bit in from the JTAG TDO and one bit out to the JTAG TDI */
            inbit = tapc_shift_bit(outbit);

            /* add the bit read into the input byte */
            /* N.B. the shift amount will always be positive, as 0 <= j < BITS_IN_BYTE */
            if (order == MSB_FIRST)
                inbyte |= (Byte) (inbit << (BITS_IN_BYTE - 1 - j));
            else
                inbyte |= (Byte) (inbit << j);
        }

        if (order == MSB_FIRST)
            to_read[nbytes - 1 - i] = inbyte;
        else
            to_read[i] = inbyte;
    }

    STATE_IS_EITHER(EXIT1_DR, EXIT1_IR);

    LEAVEMSG;
}


/* Read the JTAG Status Register.
 *
 * This indicates the status of the JTAG transaction that has been attempted.
 */
static JTAG_TransactionStatus
read_jtag_status_register(void)
{
    JTAG_RegisterContents rd, wr;
    Bit                   bit;

    ENTERMSG;

    STATE_IS_EITHER(RUN_TEST_IDLE, UPDATE_DR);

    CHANGE_STATE(1, SELECT_DR_SCAN);
    CHANGE_STATE(1, SELECT_IR_SCAN);
    CHANGE_STATE(0, CAPTURE_IR);
    CHANGE_STATE(0, SHIFT_IR);

    wr = JTAG_STATUS_REGISTER;

    tapc_shift_N_bits(BITS_IN_REGISTER_CODE, (Byte*) &wr, (Byte*) &rd, MSB_LAST);

    CHANGE_STATE(1, UPDATE_IR);
//  CHANGE_STATE(0, RUN_TEST_IDLE);
    CHANGE_STATE(1, SELECT_DR_SCAN);
    CHANGE_STATE(0, CAPTURE_DR);
    CHANGE_STATE(0, SHIFT_DR);

    /* the JTAG Status Register is read-only, and any bits shifted in are
     * ignored - hence the parameter to tapc_shift_bit is irrelevant here
     */

    /* read Stalled bit; if it is zero then keep reading */
    bit = tapc_shift_bit(0);
    if (bit)
        return STALLED;

    /* read Failed bit; if it is zero then keep reading */
    bit = tapc_shift_bit(0);
    if (bit)
        return FAILURE;

    /* read Ready bit */
    bit = tapc_shift_bit(0);
    if (bit)
        return READY;

    /* the last bit (PC_SEL) is optional */

    return NOT_READY;
}


/* Write a value to a JTAG register.
 *    enter in Update-DR/IR state or Run-Test/Idle.
 *    exit  in Update-DR
 */
static void
write_jtag_reg(JTAG_Register regnum, JTAG_RegisterContents data, unsigned int num_data_bits)
{
    Byte                  num = (Byte) regnum;
    JTAG_RegisterContents rd  = 0;

    ENTERARGS("regnum %d <== 0x%08X (:%d)", regnum, data, num_data_bits);

    STATE_IS_ONE_OF(UPDATE_DR, UPDATE_IR, RUN_TEST_IDLE);

//  CHANGE_STATE(0, RUN_TEST_IDLE);
    CHANGE_STATE(1, SELECT_DR_SCAN);
    CHANGE_STATE(1, SELECT_IR_SCAN);
    CHANGE_STATE(0, CAPTURE_IR);
    CHANGE_STATE(0, SHIFT_IR);

    tapc_shift_N_bits(BITS_IN_REGISTER_CODE, &num, (Byte*) &rd, MSB_LAST);

    CHANGE_STATE(1, UPDATE_IR);
    CHANGE_STATE(1, SELECT_DR_SCAN);
    CHANGE_STATE(0, CAPTURE_DR);
    CHANGE_STATE(0, SHIFT_DR);

    tapc_shift_N_bits(num_data_bits, (Byte*) &data, (Byte*) &rd, MSB_LAST);

    CHANGE_STATE(1, UPDATE_DR);

    DEBUG("written 0x%08X to JTAG %s register\n", data, name(regnum));

    LEAVEMSG;
}


/* Read a value from a JTAG register.
 *    enter in Update-DR/IR state
 *    exit  in Update-DR
 */
static JTAG_RegisterContents
read_jtag_reg (JTAG_Register regnum, unsigned int num_data_bits)
{
    Byte                  num = (Byte) regnum;
    JTAG_RegisterContents wr  = 0, rd = 0;

    ENTERARGS("regnum %u, %u bits", regnum, num_data_bits);

    STATE_IS_EITHER(UPDATE_DR, UPDATE_IR);

//  CHANGE_STATE(0, RUN_TEST_IDLE);
    CHANGE_STATE(1, SELECT_DR_SCAN);
    CHANGE_STATE(1, SELECT_IR_SCAN);
    CHANGE_STATE(0, CAPTURE_IR);
    CHANGE_STATE(0, SHIFT_IR);

    tapc_shift_N_bits(BITS_IN_REGISTER_CODE, &num, (Byte*) &rd, MSB_LAST);

    CHANGE_STATE(1, UPDATE_IR);

    /* JTAG registers can be read without going to Run-Test/Idle state.
     *
     * Doing CHANGE_STATE(0) would take us to Run-Test/Idle state. This would
     * make JTAG perform the transaction in the Transaction Command Register.
     * We don't want that!
     */
//  CHANGE_STATE(0, RUN_TEST_IDLE);
    CHANGE_STATE(1, SELECT_DR_SCAN);
    CHANGE_STATE(0, CAPTURE_DR);
    CHANGE_STATE(0, SHIFT_DR);

    tapc_shift_N_bits(num_data_bits, (Byte*) &wr, (Byte*) &rd, MSB_LAST);

    CHANGE_STATE(1, UPDATE_DR);

    DEBUG("read 0x%08X from JTAG %s register\n", rd, name(regnum));
    return rd;
}


/* -------------------------------------------------------------------------- */
/* 5)                        JTAG transaction functions                       */
/* -------------------------------------------------------------------------- */

static void start_jtag_transaction(JTAG_RegisterContents command,
                                   JTAG_RegisterContents address)
{
    ENTERARGS("command = %d, address = 0x%x", command, address);

    STATE_IS_ONE_OF(UPDATE_DR, UPDATE_IR, RUN_TEST_IDLE);

    /* N.B. do NOT reset the TAP Controller at the start of each transaction, as
     *      that would re-initialize the JTAG registers to their default values
     *      (whatever those might be), so invalidating the optimisation of the
     *      use of the Address and Transaction Command registers performed by
     *      this module.
     */

    if (command == READ_MEMORY_LOCATION || command == WRITE_MEMORY_LOCATION)
    {
        assert(IS_WORD_ALIGNED(address));
    }

    /* load the command that is required into the JTAG Transaction Command
     * Register, and the address into the JTAG Address Register; by keeping
     * track of the values that these registers contain, we can avoid re-loading
     * them unnecessarily, which can save time when transferring a stream of data
     */

    if (!command_register_contents_known ||
        (command_register_contents != command))
    {
        write_jtag_reg(JTAG_TRANSACTION_COMMAND_REGISTER, command, BITS_IN_COMMAND_CODE);

        command_register_contents_known = TRUE;
        command_register_contents       = command;
    }

    if (!address_register_contents_known ||
        (address_register_contents != address))
    {
        write_jtag_reg(JTAG_ADDRESS_REGISTER, address, BITS_IN_WORD);

        address_register_contents_known = TRUE;
        address_register_contents       = address;
    }

    LEAVEMSG;
}


/* if the JTAG transaction fails, this function returns the given error */
static JTAG_OperationStatus perform_jtag_transaction
                               (JTAG_TransactionType transaction,
                                JTAG_OperationStatus error)
{
    JTAG_OperationStatus result = error;
    unsigned int         tries  = 0;

    ENTERARGS("transaction: %u", transaction);

    /* this causes the TAP Controller to perform the transaction, according to
     * the contents of the JTAG Transaction Command, Address and Data registers
     */
    CHANGE_STATE(0, RUN_TEST_IDLE);

    /* poll the JTAG Status Register */
    do
    {
        JTAG_TransactionStatus status = read_jtag_status_register();

        /* the read has left the TAP Controller FSM in state Shift-DR */
        STATE_IS(SHIFT_DR);

        CHANGE_STATE(1, EXIT1_DR);
        CHANGE_STATE(1, UPDATE_DR);

        /* if the transaction is complete */
        if (status == READY)
        {
            DEBUG("*** READY\n");

            /* the value in the JTAG Address Register is incremented by four
             * (a memory access) or one (a register access) when a read/write
             * transaction has completed
             */
            address_register_contents += (transaction == Memory) ? BYTES_IN_WORD : 1;

            result = JTAG_SUCCESS;
            break;
        }

        if (status == FAILURE)
        {
            DEBUG("*** FAILED\n");
            break;
        }

        if (status == STALLED)
        {
            DEBUG("*** STALLED\n");
        }

        /* pause and re-try */
        usleep(1);
    }
    while (++tries <= arc_jtag_ops.jtag_retry_count);

    LEAVEMSG;
    return result;
}


/* -------------------------------------------------------------------------- */
/* 6)                  read/write helper functions                            */
/* -------------------------------------------------------------------------- */

/* these functions aid in reading/writing registers/memory */

static JTAG_OperationStatus
read_processor_register(ARC_RegisterNumber    regnum,
                        JTAG_RegisterContents command,
                        ARC_RegisterContents* contents)
{
    JTAG_OperationStatus status;

    /* load the number of the register that is to be read into the JTAG Address
     * Register
     */
    start_jtag_transaction(command, regnum);

    status = perform_jtag_transaction(Register, JTAG_READ_FAILURE);

    if (status == JTAG_SUCCESS)
    {
        /* read the register contents from the JTAG Data Register */
        *contents = (ARC_RegisterContents) read_jtag_reg(JTAG_DATA_REGISTER, BITS_IN_REGISTER);
    }

    return status;
}


static JTAG_OperationStatus
write_processor_register(ARC_RegisterNumber    regnum,
                         JTAG_RegisterContents command,
                         ARC_RegisterContents  contents)
{
    /* load the number of the register that is to be written into the JTAG
     * Address Register
     */
    start_jtag_transaction(command, regnum);

     /* load the new register contents into the JTAG Data Register */
    write_jtag_reg(JTAG_DATA_REGISTER, contents, BITS_IN_REGISTER);

    return perform_jtag_transaction(Register, JTAG_WRITE_FAILURE);
}


static inline JTAG_OperationStatus
read_aux_register(ARC_RegisterNumber regnum, ARC_RegisterContents* contents)
{
    return read_processor_register(regnum, READ_AUX_REGISTER, contents);
}


static inline JTAG_OperationStatus
write_aux_register(ARC_RegisterNumber regnum, ARC_RegisterContents contents)
{
    return write_processor_register(regnum, WRITE_AUX_REGISTER, contents);
}


/* split a memory chunk up into its layout - see diagram in file header */
static Layout split(ARC_Address addr, unsigned int bytes)
{
    unsigned int gap = addr % BYTES_IN_WORD;
    Layout       layout;

    layout.leading_bytes  = (gap == 0) ? 0 : (BYTES_IN_WORD - gap);
    layout.trailing_bytes = (addr + bytes) % BYTES_IN_WORD;
    layout.words          = (bytes - layout.leading_bytes
                                   - layout.trailing_bytes) / BYTES_IN_WORD;
    return layout;
}


/* read a word of data from memory; the given address must be word-aligned */
static unsigned int
read_word(ARC_Address addr, ARC_Word* data)
{
    JTAG_OperationStatus status;

    assert(IS_WORD_ALIGNED(addr));

    /* load the address of the memory word that is to be read into the JTAG
     * Address Register
     */
    start_jtag_transaction(READ_MEMORY_LOCATION, addr);

    status = perform_jtag_transaction(Memory, JTAG_READ_FAILURE);

    if (status == JTAG_SUCCESS)
    {
        /* read the data from the JTAG Data Register */
        *data = (ARC_Word) read_jtag_reg(JTAG_DATA_REGISTER, BITS_IN_WORD);

        DEBUG("read 0x%08X\n", *data);

        return BYTES_IN_WORD;
    }

    /* failed: no data read */
    return 0;
}


/* write a word of data to memory; the given address must be word-aligned */
static unsigned int
write_word(ARC_Address addr, ARC_Word data)
{
    assert(IS_WORD_ALIGNED(addr));

    /* load the address of the memory word that is to be written into the JTAG
     * Address Register
     */
    start_jtag_transaction(WRITE_MEMORY_LOCATION, addr);

    /* load the data to be written into the JTAG Data Register */
    write_jtag_reg(JTAG_DATA_REGISTER, data, BITS_IN_WORD);

    if (perform_jtag_transaction(Memory, JTAG_WRITE_FAILURE) == JTAG_SUCCESS)
        return BYTES_IN_WORD;

    /* failed: no data written */
    return 0;
}


/* read part of a word of data from memory; the given address must be word-aligned */
static unsigned int
read_partial_word(ARC_Address  addr,
                  ARC_Byte*    data,
                  unsigned int bytes,
                  unsigned int offset)   // offset of required bytes within word
{
    WordBytes value;

    /* read the word: only some bytes of this are required */
    if (read_word(addr, &value.word) > 0)
    {
        unsigned int i;

        for (i = 0; i < bytes; i++)
            data[i] = value.byte[offset + i];

        /* have read the specified number of bytes */
        return bytes;
    }

    /* failed: no data read */
    return 0;
}


/* write part of a word of data to memory; the given address must be word-aligned */
static unsigned int
write_partial_word(ARC_Address  addr,
                   ARC_Byte*    data,
                   unsigned int bytes,
                   unsigned int offset)   // offset of required bytes within word
{
    WordBytes value;

    /* first read the word of memory that will be overwritten */
    if (read_word(addr, &value.word) > 0)
    {
        unsigned int i;

        /* then replace the bytes in that word that are to be written */
        for (i = 0; i < bytes; i++)
            value.byte[offset + i] = data[i];

        /* now write it! */
        if (write_word(addr, value.word) > 0)
            /* have written the specified number of bytes */
            return bytes;
    }

    /* failed: no data written */
    return 0;
}


/* -------------------------------------------------------------------------- */
/* 7)                            main operations                              */
/* -------------------------------------------------------------------------- */

/* these are the functions that are called from outside this module via the
 * pointers in the arc_jtag_ops global object
 *
 * N.B. none of these functions are called from within this module
 */

/* Read a processor core register */
static JTAG_OperationStatus
jtag_read_core_reg(ARC_RegisterNumber regnum, ARC_RegisterContents* contents)
{
    ENTERARGS("regnum %d", regnum);

    check_and_raise_error_if_JTAG_closed();

    return read_processor_register(regnum, READ_CORE_REGISTER, contents);
}


/* Write a processor core register */
static JTAG_OperationStatus
jtag_write_core_reg(ARC_RegisterNumber regnum, ARC_RegisterContents contents)
{
    ENTERARGS("regnum %d", regnum);

    check_and_raise_error_if_JTAG_closed();

    return write_processor_register(regnum, WRITE_CORE_REGISTER, contents);
}


/* Read a processor auxiliary register */
static JTAG_OperationStatus
jtag_read_aux_reg(ARC_RegisterNumber regnum, ARC_RegisterContents* contents)
{
    ENTERARGS("regnum %d", regnum);

    check_and_raise_error_if_JTAG_closed();

    return read_aux_register(regnum, contents);
}


/* Write a processor auxiliary register */
static JTAG_OperationStatus
jtag_write_aux_reg(ARC_RegisterNumber regnum, ARC_RegisterContents contents)
{
    ENTERARGS("regnum %d", regnum);

    check_and_raise_error_if_JTAG_closed();

    return write_processor_register(regnum, WRITE_AUX_REGISTER, contents);
}


/* Read a word of data from memory; the given address must be word-aligned.
 * Returns number of bytes read.
 */
static unsigned int
jtag_read_word(ARC_Address addr, ARC_Word* data)
{
    ENTERARGS("addr 0x%08X", addr);

    check_and_raise_error_if_JTAG_closed();

    return read_word(addr, data);
}


/* Write a word of data to memory; the given address must be word-aligned.
 * Returns number of bytes written.
 */
static unsigned int
jtag_write_word(ARC_Address addr, ARC_Word data)
{
    ENTERARGS("addr 0x%08X, data 0x%08X", addr, data);

    check_and_raise_error_if_JTAG_closed();

    return write_word(addr, data);
}


/* Read a chunk of data from target memory.
 * Returns number of bytes read.
 */
static unsigned int
jtag_read_chunk(ARC_Address addr, ARC_Byte* data, unsigned int bytes)
{
    unsigned int gap        = addr % BYTES_IN_WORD;
    unsigned int total_read = 0;

    ENTERARGS("addr 0x%08X, bytes %u", addr, bytes);

    check_and_raise_error_if_JTAG_closed();

    /* special fast case for reading a single word */
    if (gap == 0 && bytes == BYTES_IN_WORD)
    {
        DEBUG("read single word\n");

        // N.B. assumes that 'data' is word-aligned, or that host does not care!
        total_read = read_word(addr, (ARC_Word*) data);
    }
    /* pathological case: bytes in middle of word */
    else if (gap > 0 && gap + bytes < BYTES_IN_WORD)
    {
        DEBUG("read pathological\n");

        total_read = read_partial_word(addr - gap,   // word-aligned address
                                       data,
                                       bytes,
                                       gap);
    }
    else /* general case */
    {
        Layout chunk = split(addr, bytes);

        if (chunk.leading_bytes > 0)
        {
            /* read the first few bytes */
            total_read = read_partial_word(addr - gap,   // word-aligned address
                                           data,
                                           chunk.leading_bytes,
                                           gap);
            data += chunk.leading_bytes;
            addr += chunk.leading_bytes;
        }

        if (chunk.words > 0)
        {
            /* load the start address of the memory chunk that is to be read
             * into the JTAG Address Register
             */
            start_jtag_transaction(READ_MEMORY_LOCATION, addr);

            /* read all the complete words of data */
            while (chunk.words--)
            {
                ARC_Word word;

                /* read the next word of data - this increments the address in
                 * the JTAG Address Register by the word size, so the register
                 * does not have to be re-loaded with the next address
                 */
                if (perform_jtag_transaction(Memory, JTAG_READ_FAILURE) != JTAG_SUCCESS)
                {
                    DEBUG("FAIL: read %u bytes\n", total_read);

                    /* failed - just return amount of data read so far */
                    return total_read;
                }

                /* read the word of data from the JTAG Data Register */
                word = (ARC_Word) read_jtag_reg(JTAG_DATA_REGISTER, BITS_IN_WORD);

                /* copy it into the buffer
                 * (byte-by-byte copy means that alignment does not matter)
                 */
                (void) memcpy(data, &word, BYTES_IN_WORD);

                total_read += BYTES_IN_WORD;
                addr       += BYTES_IN_WORD;
                data       += BYTES_IN_WORD;
           }
        }

        if (chunk.trailing_bytes > 0)
        {
            /* read the last few bytes of data */
            total_read += read_partial_word(addr,     // word-aligned address
                                            data,
                                            chunk.trailing_bytes,
                                            0);
        }
    }

    DEBUG("read %u bytes\n", total_read);

    return total_read;
}


/* Write a chunk of data to target memory.
 * Returns number of bytes written.
 */
static unsigned int
jtag_write_chunk(ARC_Address addr, ARC_Byte* data, unsigned int bytes)
{
    unsigned int gap           = addr % BYTES_IN_WORD;
    unsigned int total_written = 0;

    ENTERARGS("addr 0x%08X, bytes %u", addr, bytes);

    check_and_raise_error_if_JTAG_closed();

    /* special fast case for writing a single word */
    if (gap == 0 && bytes == BYTES_IN_WORD)
    {
        DEBUG("write single word (%02X %02X %02X %02X)\n", data[0], data[1], data[2], data[3]);

        // N.B. assumes that 'data' is word-aligned, or that host does not care!
        total_written = write_word(addr, *((ARC_Word*) data));
    }
    /* pathological case: bytes in middle of word */
    else if (gap > 0 && gap + bytes < BYTES_IN_WORD)
    {
        DEBUG("write pathological\n");

        total_written = write_partial_word(addr - gap,   // word-aligned address
                                           data,
                                           bytes,
                                           gap);
    }
    else /* general case */
    {
        Layout chunk = split(addr, bytes);

        if (chunk.leading_bytes > 0)
        {
            /* write the first few bytes */
            total_written = write_partial_word(addr - gap,   // word-aligned address
                                               data,
                                               chunk.leading_bytes,
                                               gap);
            data += chunk.leading_bytes;
            addr += chunk.leading_bytes;
        }

        if (chunk.words > 0)
        {
            /* load the start address of the memory chunk that is to be written
             * into the JTAG Address Register
             */
            start_jtag_transaction(WRITE_MEMORY_LOCATION, addr);

            /* write all the complete words of data */
            while (chunk.words--)
            {
                ARC_Word word;

                /* copy the next word of data from the buffer
                 * (byte-by-byte copy means that alignment does not matter)
                 */
                (void) memcpy(&word, data, BYTES_IN_WORD);

                /* load the word of data into the JTAG Data Register */
                write_jtag_reg(JTAG_DATA_REGISTER, word, BITS_IN_WORD);

                data += BYTES_IN_WORD;

                /* write the word - this increments the address in the JTAG
                 * Address Register by the word size, so the register does not
                 * have to be re-loaded with the next address
                 */
                if (perform_jtag_transaction(Memory, JTAG_WRITE_FAILURE) != JTAG_SUCCESS)
                {
                    DEBUG("FAIL: written %u bytes\n", total_written);

                    /* failed - just return amount of data written so far */
                    return total_written;
                }

                total_written += BYTES_IN_WORD;
                addr          += BYTES_IN_WORD;
            }
        }

        if (chunk.trailing_bytes > 0)
        {
            /* write the last few bytes of data */
            total_written += write_partial_word(addr,    // word-aligned address
                                                data,
                                                chunk.trailing_bytes,
                                                0);
        }
    }

    DEBUG("written %u bytes\n", total_written);

    return total_written;
}


/* write a repeated pattern of data to memory;
 * the start of each pattern is always word-aligned, so if the given address is
 * not word-aligned, the first partial word written will contain trailing bytes
 * of the pattern
 */
static unsigned int
jtag_write_pattern(ARC_Address addr, ARC_Word pattern, unsigned int bytes)
{
    unsigned int gap           = addr % BYTES_IN_WORD;
    unsigned int total_written = 0;

    ENTERARGS("addr 0x%08X, pattern 0x%08X, bytes %u", addr, pattern, bytes);

    check_and_raise_error_if_JTAG_closed();

    /* special fast case for writing a single word */
    if (gap == 0 && bytes == BYTES_IN_WORD)
    {
        DEBUG("write single word (%08X)\n", pattern);

        total_written = write_word(addr, pattern);
    }
    /* pathological case: bytes in middle of word */
    else if (gap > 0 && gap + bytes < BYTES_IN_WORD)
    {
        DEBUG("write pathological\n");

        total_written = write_partial_word(addr - gap,   // word-aligned address
                                           ((ARC_Byte*) &pattern) + gap ,
                                           bytes,
                                           gap);
    }
    else /* general case */
    {
        Layout chunk = split(addr, bytes);

        if (chunk.leading_bytes > 0)
        {
            /* write the first few bytes */
            total_written = write_partial_word(addr - gap,   // word-aligned address
                                               ((ARC_Byte*) &pattern) + gap ,
                                               chunk.leading_bytes,
                                               gap);

            addr += chunk.leading_bytes;
        }

        if (chunk.words > 0)
        {
            /* load the start address of the memory chunk that is to be written
             * into the JTAG Address Register
             */
            start_jtag_transaction(WRITE_MEMORY_LOCATION, addr);

//          /* load the pattern into the JTAG Data Register */
//          write_jtag_reg(JTAG_DATA_REGISTER, pattern, BITS_IN_WORD);

            /* write all the complete words of data */
            while (chunk.words--)
            {
                /* load the pattern into the JTAG Data Register 
                 * N.B. we would really like to do this before the loop, but
                 *      there seems to be undocumented behaviour of the JTAG
                 *      mechanism that the Data Register is set to zero after
                 *      a write operation!
                 */
                write_jtag_reg(JTAG_DATA_REGISTER, pattern, BITS_IN_WORD);

                /* write the word - this increments the address in the JTAG
                 * Address Register by the word size, so the register does not
                 * have to be re-loaded with the next address
                 */
                if (perform_jtag_transaction(Memory, JTAG_WRITE_FAILURE) != JTAG_SUCCESS)
                {
                    DEBUG("FAIL: written %u bytes\n", total_written);

                    /* failed - just return amount of data written so far */
                    return total_written;
                }

#if 0
                {
                    /* read the data from the JTAG Data Register */
                    ARC_Word back = (ARC_Word) read_jtag_reg(JTAG_DATA_REGISTER, BITS_IN_WORD);
                    /* is the data still in the register? */
                    assert(back == pattern);
                }
#endif

                total_written += BYTES_IN_WORD;
                addr          += BYTES_IN_WORD;
            }
        }

        if (chunk.trailing_bytes > 0)
        {
            /* write the last few bytes of data */
            total_written += write_partial_word(addr,    // word-aligned address
                                                ((ARC_Byte*) &pattern),
                                                chunk.trailing_bytes,
                                                0);
        }
    }

    DEBUG("written %u bytes\n", total_written);

    return total_written;
}



/* Open the JTAG interface.
 * Returns TRUE for success.
 */
static Boolean
jtag_open(void)
{
    Boolean      first_1        = TRUE;
    Boolean      first_2        = TRUE;
    Boolean      halt_attempted = FALSE;
    unsigned int tries          = 0;
    int          retval;

    if (arc_jtag_ops.jtag_status == JTAG_OPENED)
        // success - already open!
        return TRUE;

    retval = gpio_setup();

    if (retval != 0)
    {
        warning("Unable to open JTAG port (device " GPIO_DEVICE "): %s\n",
                strerror(retval));
        return FALSE;
    }

    arc_jtag_ops.jtag_status = JTAG_OPENED;

    /* we do not yet know what processor is on the target */
    ARC_processor = UNSUPPORTED;

    /* The Test Clock signal is active low (i.e. the signal is active when the
     * corresponding bit written to the control bit is 0; so initialize the bit
     * in the control port value to 1 so that the signal is initially not active.
     */
    control_port_value = (Byte) JTAG_TCK;
    data_port_value    = (Byte) 0;
    port_error         = FALSE;

    tapc_reset();

    /* try to ensure that the processor is halted
     *
     * N.B. unfortunately, if the gpio driver module has been installed on the
     *      host machine, the gpio read/write operations appear to work even if
     *      the host is NOT physically connected to the JTAG target!
     *
     *      There does not appear to be any way of detecting that situation -
     *      all we can do is bale out if we have not succeded in reading the
     *      STATUS32 register after the required number of retries!
     */
    do
    {
        JTAG_RegisterContents status = 0;

        /* read the status32 register here to check if the halt bit is set */
        if (read_aux_register(ARC_HW_STATUS32_REGNUM, &status) == JTAG_READ_FAILURE)
        {
            if (first_1)
            {
                warning("Failure reading STATUS32 auxiliary register\n");
                first_1 = FALSE;
            }
        }
        else
        {
            if (status & STATUS32_HALT)
            {
                printf_filtered("Processor halted.\n");
                // success!
                return TRUE;
            }

            if (first_2)
            {
                printf_filtered("Processor running. Trying to halt it...\n");
                first_2 = FALSE;
            }

            if (write_aux_register(ARC_HW_DEBUG_REGNUM, DEBUG_FORCE_HALT) == JTAG_WRITE_FAILURE)
                warning("Failure setting FH bit in DEBUG auxiliary register\n");
            else
                halt_attempted = TRUE;
        }

        /* just in case we actually did fail to read/write the port */
        if (port_error)
        {
            warning("Error in accessing parallel port - check connection to board.\n");
            return FALSE;
        }
    }
    while (++tries <= arc_jtag_ops.jtag_retry_count);

    if (halt_attempted)
        printf_filtered("Could not halt processor!\n");
    else
        printf_filtered("Could not connect to processor!\n");

    return FALSE;
}


/* close the JTAG interface */
static void
jtag_close(void)
{
    if (arc_jtag_ops.jtag_status == JTAG_OPENED)
    {
        tapc_reset();

        /* close file descriptor opened for communication with gpio driver */
        (void) close(gpio_driver);

        arc_jtag_ops.jtag_status = JTAG_CLOSED;

        ARC_processor = UNSUPPORTED;

#ifdef STATE_MACHINE_DEBUG
        current_state = UNDEFINED;
#endif

        DEBUG("arc-jtag closed\n");
    }
}


/* wait for the target to halt */
static void
jtag_wait(void)
{
    sigset_t block_mask;

    check_and_raise_error_if_JTAG_closed();

    /* we want to be able to ignore SIGINTs */
    (void) sigemptyset(&block_mask);
    (void) sigaddset  (&block_mask, SIGINT);

    /* busy wait until processor has halted */
    do
    {
        JTAG_RegisterContents status32;

        /* ignore any user interrupt whilst we are waiting for the processor to halt */
        (void) sigprocmask(SIG_BLOCK, &block_mask, NULL);

        if (read_aux_register(ARC_HW_STATUS32_REGNUM, &status32) == JTAG_READ_FAILURE)
            warning("Failure reading STATUS32 auxiliary register\n");
        else
        {
#if 0
printf("status32: %08X\n", status32);

{
        JTAG_RegisterContents PC;

        if (read_aux_register(ARC_HW_PC_REGNUM, &PC) == JTAG_READ_FAILURE)
            warning("Failure reading PC auxiliary register\n");
        else
            printf("PC: %08X\n", PC);
}
#endif

            if (status32 & STATUS32_HALT)
            {
                 (void) sigprocmask(SIG_UNBLOCK, &block_mask, NULL);
                 break;
            }
        }

        (void) sigprocmask(SIG_UNBLOCK, &block_mask, NULL);
    } while (TRUE);


    /* busy wait for any delayed load to complete */
    while (TRUE)
    {
        JTAG_RegisterContents debug;

        if (read_aux_register(ARC_HW_DEBUG_REGNUM, &debug) == JTAG_READ_FAILURE)
            warning("Failure reading DEBUG auxiliary register\n");
        else
            if (!(debug & DEBUG_LOAD_PENDING))
                break;
    }
}


/* reset the target board */
static void
jtag_reset_board(void)
{
   check_and_raise_error_if_JTAG_closed();

   /* force the processor to halt */
   if (write_aux_register(ARC_HW_DEBUG_REGNUM, DEBUG_FORCE_HALT) == JTAG_WRITE_FAILURE)
       warning("Failure writing to DEBUG auxiliary register\n");

   /* Writing 9 did not work. But that's what the manual says. Hmmm. */
// gpio_write (CONTROL_PORT, 9);

   /* what is this for? */
   gpio_write(CONTROL_PORT, (Byte) JTAG_TCK);
   gpio_write(CONTROL_PORT, (Byte) 0xD);
   gpio_write(CONTROL_PORT, (Byte) JTAG_TCK);
   gpio_write(DATA_PORT,    (Byte) 0);
   gpio_write(DATA_PORT,    (Byte) JTAG_TMS);
   gpio_write(DATA_PORT,    (Byte) 0);

   tapc_reset();
}


/* -------------------------------------------------------------------------- */
/*                               externally visible functions                 */
/* -------------------------------------------------------------------------- */

/* return the processor variant that is connected */
ARC_ProcessorVersion arc_get_architecture(void)
{
    ENTERMSG;

    check_and_raise_error_if_JTAG_closed();

    if (ARC_processor == UNSUPPORTED)
    {
        JTAG_RegisterContents value;

        if (read_aux_register(ARC_HW_IDENTITY_REGNUM, &value) == JTAG_READ_FAILURE)
            warning("Failure reading from IDENTITY auxiliary register");
        else
        {
            /* get the processor version number */
            value &= IDENTITY_ARCVER;

            if ((value >= 0x30) && (value <= 0x3f))
                ARC_processor = ARC700;
            else if ((value >= 0x20) && (value <= 0x2f))
                ARC_processor = ARC600;
            else if ((value >= 0x10) && (value <= 0x1f))
                ARC_processor = A5;
            else if (value <= 0x0f)
                ARC_processor = A4;
            else
                warning("Unsupported Processor Version 0x%x\n", value);
       }
   }

   return ARC_processor;
}


/* initialize the arc_jtag_ops global variable */
void
_initialize_arc_jtag_ops(void)
{
    ENTERMSG;

    arc_jtag_ops.name                     = NULL;
    arc_jtag_ops.jtag_status              = JTAG_CLOSED;
    arc_jtag_ops.jtag_state_machine_debug = FALSE;
    arc_jtag_ops.jtag_retry_count         = 50;

    arc_jtag_ops.jtag_open                 = jtag_open;
    arc_jtag_ops.jtag_close                = jtag_close;
    arc_jtag_ops.jtag_memory_read_word     = jtag_read_word;
    arc_jtag_ops.jtag_memory_write_word    = jtag_write_word;
    arc_jtag_ops.jtag_memory_read_chunk    = jtag_read_chunk;
    arc_jtag_ops.jtag_memory_write_chunk   = jtag_write_chunk;
    arc_jtag_ops.jtag_memory_write_pattern = jtag_write_pattern;
    arc_jtag_ops.jtag_read_aux_reg         = jtag_read_aux_reg;
    arc_jtag_ops.jtag_write_aux_reg        = jtag_write_aux_reg;
    arc_jtag_ops.jtag_read_core_reg        = jtag_read_core_reg;
    arc_jtag_ops.jtag_write_core_reg       = jtag_write_core_reg;
    arc_jtag_ops.jtag_wait                 = jtag_wait;
    arc_jtag_ops.jtag_reset_board          = jtag_reset_board;
}

/******************************************************************************/
