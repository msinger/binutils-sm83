/* Print Sharp SM83 (Game Boy) instructions
   Copyright (C) 2005-2026 Free Software Foundation, Inc.
   Based on z80-dis.c

   This file is part of the GNU opcodes library.

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include "sysdep.h"
#include "disassemble.h"
#include <stdio.h>

int
print_insn_sm83 (bfd_vma addr, disassemble_info *info);

struct buffer
{
  bfd_vma base;
  int n_fetch;
  int n_used;
  signed char data[4];
};

typedef int (*func)(struct buffer *, disassemble_info *, const char *);

struct tab_elt
{
  unsigned char val;
  unsigned char mask;
  func          fp;
  const char    *text;
};

#define TXTSIZ 24
/* Names of 16-bit registers.  */
static const char *rr_str[] = { "bc", "de", "hl", "sp" };
/* Names of 8-bit registers.  */
static const char *r_str[]  = { "b", "c", "d", "e", "h", "l", "(hl)", "a" };
/* Texts for condition codes.  */
static const char *cc_str[] = { "nz", "z", "nc", "c" };
/* Instruction names for 8-bit arithmetic, operand "a" is often implicit */
static const char *arit_str[] =
{
  "add a,", "adc a,", "sub ", "sbc a,", "and ", "xor ", "or ", "cp "
};

static int
fetch_data (struct buffer *buf, disassemble_info *info, int n)
{
  int r;

  if (buf->n_fetch + n > (int)sizeof (buf->data))
    abort ();

  r = info->read_memory_func (buf->base + buf->n_fetch,
			      (unsigned char*) buf->data + buf->n_fetch,
			      n, info);
  if (r == 0)
    buf->n_fetch += n;
  else
    info->memory_error_func (r, buf->base + buf->n_fetch, info);
  return !r;
}

static int
prt (struct buffer *buf, disassemble_info *info, const char *txt)
{
  info->fprintf_func (info->stream, "%s", txt);
  buf->n_used = buf->n_fetch;
  return 1;
}

static int
prt_e (struct buffer *buf, disassemble_info *info, const char *txt)
{
  char e;
  int target_addr;

  if (fetch_data (buf, info, 1))
    {
      e = buf->data[1];
      target_addr = (buf->base + 2 + e) & 0xffff;
      buf->n_used = buf->n_fetch;
      info->fprintf_func (info->stream, "%s0x%04x", txt, target_addr);
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
jr_cc (struct buffer *buf, disassemble_info *info, const char *txt)
{
  char mytxt[TXTSIZ];

  snprintf (mytxt, TXTSIZ, txt, cc_str[(buf->data[0] >> 3) & 3]);
  return prt_e (buf, info, mytxt);
}

static int
prt_nn (struct buffer *buf, disassemble_info *info, const char *txt)
{
  int nn;
  unsigned char *p;

  p = (unsigned char*) buf->data + buf->n_fetch;
  if (fetch_data (buf, info, 2))
    {
      nn = p[0] + (p[1] << 8);
      info->fprintf_func (info->stream, txt, nn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;
  return buf->n_used;
}

static int
prt_rr_nn (struct buffer *buf, disassemble_info *info, const char *txt)
{
  char mytxt[TXTSIZ];
  int rr;

  rr = (buf->data[buf->n_fetch - 1] >> 4) & 3;
  snprintf (mytxt, TXTSIZ, txt, rr_str[rr]);
  return prt_nn (buf, info, mytxt);
}

static int
prt_rr (struct buffer *buf, disassemble_info *info, const char *txt)
{
  info->fprintf_func (info->stream, "%s%s", txt,
		      rr_str[(buf->data[buf->n_fetch - 1] >> 4) & 3]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_n (struct buffer *buf, disassemble_info *info, const char *txt)
{
  int n;
  unsigned char *p;

  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      n = p[0];
      info->fprintf_func (info->stream, txt, n);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
ld_r_n (struct buffer *buf, disassemble_info *info, const char *txt)
{
  char mytxt[TXTSIZ];

  snprintf (mytxt, TXTSIZ, txt, r_str[(buf->data[buf->n_fetch - 1] >> 3) & 7]);
  return prt_n (buf, info, mytxt);
}

static int
prt_r (struct buffer *buf, disassemble_info *info, const char *txt)
{
  info->fprintf_func (info->stream, txt,
		      r_str[(buf->data[buf->n_fetch - 1] >> 3) & 7]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
ld_r_r (struct buffer *buf, disassemble_info *info, const char *txt)
{
  info->fprintf_func (info->stream, txt,
		      r_str[(buf->data[buf->n_fetch - 1] >> 3) & 7],
		      r_str[buf->data[buf->n_fetch - 1] & 7]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_d (struct buffer *buf, disassemble_info *info, const char *txt)
{
  int d;
  signed char *p;

  p = buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      d = p[0];
      info->fprintf_func (info->stream, txt, d);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
arit_r (struct buffer *buf, disassemble_info *info, const char *txt)
{
  info->fprintf_func (info->stream, txt,
                      arit_str[(buf->data[buf->n_fetch - 1] >> 3) & 7],
                      r_str[buf->data[buf->n_fetch - 1] & 7]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_cc (struct buffer *buf, disassemble_info *info, const char *txt)
{
  info->fprintf_func (info->stream, "%s%s", txt,
		      cc_str[(buf->data[0] >> 3) & 3]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
pop_rr (struct buffer *buf, disassemble_info *info, const char *txt)
{
  static char *rr_stack[] = { "bc", "de", "hl", "af" };

  info->fprintf_func (info->stream, "%s %s", txt,
		      rr_stack[(buf->data[0] >> 4) & 3]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
jp_cc_nn (struct buffer *buf, disassemble_info *info, const char *txt)
{
  char mytxt[TXTSIZ];

  snprintf (mytxt,TXTSIZ,
	    "%s%s,0x%%04x", txt, cc_str[(buf->data[0] >> 3) & 3]);
  return prt_nn (buf, info, mytxt);
}

static int
arit_n (struct buffer *buf, disassemble_info *info, const char *txt)
{
  char mytxt[TXTSIZ];

  snprintf (mytxt,TXTSIZ, txt, arit_str[(buf->data[0] >> 3) & 7]);
  return prt_n (buf, info, mytxt);
}

static int
rst (struct buffer *buf, disassemble_info *info, const char *txt)
{
  info->fprintf_func (info->stream, txt, buf->data[0] & 0x38);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
dump (struct buffer *buf, disassemble_info *info, const char *txt)
{
  int i;

  info->fprintf_func (info->stream, "defb ");
  for (i = 0; txt[i]; ++i)
    info->fprintf_func (info->stream, i ? ", 0x%02x" : "0x%02x",
			(unsigned)(unsigned char) buf->data[i]);
  buf->n_used = i;
  return buf->n_used;
}

/* Instruction names for the instructions addressing single bits.  */
static char *cb1_str[] = { "", "bit", "res", "set"};
/* Instruction names for shifts and rotates.  */
static char *cb2_str[] =
{
  "rlc", "rrc", "rl", "rr", "sla", "sra", "swap", "srl"
};

static int
pref_cb (struct buffer *buf, disassemble_info *info,
         const char *txt ATTRIBUTE_UNUSED)
{
  if (fetch_data (buf, info, 1))
    {
      buf->n_used = 2;
      if ((buf->data[1] & 0xc0) == 0)
	info->fprintf_func (info->stream, "%s %s",
			    cb2_str[(buf->data[1] >> 3) & 7],
			    r_str[buf->data[1] & 7]);
      else
	info->fprintf_func (info->stream, "%s %d,%s",
			    cb1_str[(buf->data[1] >> 6) & 3],
			    (buf->data[1] >> 3) & 7,
			    r_str[buf->data[1] & 7]);
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

/* Table to disassemble machine codes without prefix. */
static const struct tab_elt opc_main[] =
{
  { 0x00, 0xFF, prt, "nop" },
  { 0x01, 0xCF, prt_rr_nn, "ld %s,0x%%04x" },
  { 0x02, 0xFF, prt, "ld (bc),a" },
  { 0x03, 0xCF, prt_rr, "inc " },
  { 0x04, 0xC7, prt_r, "inc %s" },
  { 0x05, 0xC7, prt_r, "dec %s" },
  { 0x06, 0xC7, ld_r_n, "ld %s,0x%%02x" },
  { 0x07, 0xFF, prt, "rlca" },
  { 0x08, 0xFF, prt_nn, "ld (0x%04x),sp" },
  { 0x09, 0xCF, prt_rr, "add hl," },
  { 0x0A, 0xFF, prt, "ld a,(bc)" },
  { 0x0B, 0xCF, prt_rr, "dec " },
  { 0x0F, 0xFF, prt, "rrca" },
  { 0x10, 0xFF, prt, "stop" },
  { 0x12, 0xFF, prt, "ld (de),a" },
  { 0x17, 0xFF, prt, "rla" },
  { 0x18, 0xFF, prt_e, "jr " },
  { 0x1A, 0xFF, prt, "ld a,(de)" },
  { 0x1F, 0xFF, prt, "rra" },
  { 0x20, 0xE7, jr_cc, "jr %s," },
  { 0x22, 0xFF, prt, "ld (hli),a" },
  { 0x27, 0xFF, prt, "daa" },
  { 0x2A, 0xFF, prt, "ld a,(hli)" },
  { 0x2F, 0xFF, prt, "cpl" },
  { 0x32, 0xFF, prt, "ld (hld),a" },
  { 0x37, 0xFF, prt, "scf" },
  { 0x3A, 0xFF, prt, "ld a,(hld)" },
  { 0x3F, 0xFF, prt, "ccf" },
  { 0x76, 0xFF, prt, "halt" },
  { 0x40, 0xC0, ld_r_r, "ld %s,%s" },
  { 0x80, 0xC0, arit_r, "%s%s" },
  { 0xC0, 0xE7, prt_cc, "ret " },
  { 0xC1, 0xCF, pop_rr, "pop" },
  { 0xC2, 0xE7, jp_cc_nn, "jp " },
  { 0xC3, 0xFF, prt_nn, "jp 0x%04x" },
  { 0xC4, 0xE7, jp_cc_nn, "call " },
  { 0xC5, 0xCF, pop_rr, "push" },
  { 0xC6, 0xC7, arit_n, "%s0x%%02x" },
  { 0xC7, 0xC7, rst, "rst 0x%02x" },
  { 0xC9, 0xFF, prt, "ret" },
  { 0xCB, 0xFF, pref_cb, "" },
  { 0xCD, 0xFF, prt_nn, "call 0x%04x" },
  { 0xD9, 0xFF, prt, "reti" },
  { 0xE0, 0xFF, prt_n, "ld (0x%02x),a" },
  { 0xE2, 0xFF, prt, "ld (c),a" },
  { 0xE8, 0xFF, prt_d, "add sp,%d" },
  { 0xE9, 0xFF, prt, "jp (hl)" },
  { 0xEA, 0xFF, prt_nn, "ldx (0x%04x),a" },
  { 0xF0, 0xFF, prt_n, "ld a,(0x%02x)" },
  { 0xF2, 0xFF, prt, "ld a,(c)" },
  { 0xF3, 0xFF, prt, "di" },
  { 0xF8, 0xFF, prt_d, "ldhl sp,%d" },
  { 0xF9, 0xFF, prt, "ld sp,hl" },
  { 0xFA, 0xFF, prt_nn, "ldx a,(0x%04x)" },
  { 0xFB, 0xFF, prt, "ei" },
  { 0x00, 0x00, dump, "?" },
};

static int
print_insn_sm83_buf (struct buffer *buf, disassemble_info *info)
{
  const struct tab_elt *p;

  buf->n_fetch = 0;
  buf->n_used = 0;
  if (! fetch_data (buf, info, 1))
    return -1;

  for (p = opc_main; p->val != (buf->data[0] & p->mask); ++p)
    ;
  p->fp (buf, info, p->text);

  return buf->n_used;
}

int
print_insn_sm83 (bfd_vma addr, disassemble_info *info)
{
  struct buffer buf;

  buf.base = addr;
  info->bytes_per_line = 4;

  return print_insn_sm83_buf (&buf, info);
}
