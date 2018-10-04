/* coff information for Sharp LR35902
   Copyright (C) 2005-2018 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.  */

#define L_LNNO_SIZE 4
#include "coff/external.h"

/* Type of cpu is stored in flags. */
#define F_MACHMASK 0xF000

/* LR35902 COFF encodes the section alignment in the section header flags. */
#define COFF_ALIGN_IN_SECTION_HEADER 1
#define COFF_ALIGN_IN_S_FLAGS 1
#define F_ALGNMASK 0x0F00
/* requires a power-of-two argument */
#define COFF_ENCODE_ALIGNMENT(S,X) ((S).s_flags |= (((unsigned)(X)&0xF)<<8))
/* result is a power of two */
#define COFF_DECODE_ALIGNMENT(X) (((X)>>8)&0xF)

#define LR35902MAGIC 0x8C3E

#define LR35902BADMAG(x) (((x).f_magic != LR35902MAGIC))

/* Relocation directives. */

/* This format actually has more bits than we need. */

struct external_reloc
{
  char r_vaddr[4];
  char r_symndx[4];
  char r_offset[4];
  char r_type[2];
  char r_stuff[2];
};

#define RELOC struct external_reloc
#define RELSZ 16
