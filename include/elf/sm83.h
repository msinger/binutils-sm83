/* Sharp SM83 (Game Boy) ELF support for BFD.
   Copyright (C) 1999-2026 Free Software Foundation, Inc.
   Based on elf/z80.h

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef _ELF_SM83_H
#define _ELF_SM83_H

#include "elf/reloc-macros.h"

#define EF_SM83_MACH_DEFAULT 0x00
#define EF_SM83_MACH_MSK     0xff

/* Relocations.  */
START_RELOC_NUMBERS (elf_sm83_reloc_type)
     RELOC_NUMBER (R_SM83_NONE,		0)
     RELOC_NUMBER (R_SM83_8,		1)
     RELOC_NUMBER (R_SM83_8_DIS,	2)
     RELOC_NUMBER (R_SM83_8_PCREL,	3)
     RELOC_NUMBER (R_SM83_16,		4)
     RELOC_NUMBER (R_SM83_24,		5)
     RELOC_NUMBER (R_SM83_32,		6)
     RELOC_NUMBER (R_SM83_BYTE0,	7)
     RELOC_NUMBER (R_SM83_BYTE1,	8)
     RELOC_NUMBER (R_SM83_BYTE2,	9)
     RELOC_NUMBER (R_SM83_BYTE3,	10)
     RELOC_NUMBER (R_SM83_WORD0,	11)
     RELOC_NUMBER (R_SM83_WORD1,	12)
END_RELOC_NUMBERS (R_SM83_max)

#endif /* _ELF_SM83_H */
