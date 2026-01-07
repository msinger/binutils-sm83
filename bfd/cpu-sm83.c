/* BFD library support routines for the Sharp SM83 (Game Boy) architecture.
   Copyright (C) 2005-2026 Free Software Foundation, Inc.
   Based on cpu-z80.c

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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include "sysdep.h"
#include "bfd.h"
#include "libbfd.h"

const bfd_arch_info_type bfd_sm83_arch;

/* This routine is provided two arch_infos and
   returns whether they'd be compatible.  */

static const bfd_arch_info_type *
compatible (const bfd_arch_info_type *a, const bfd_arch_info_type *b)
{
  if (a->arch != b->arch || a->arch != bfd_arch_sm83)
    return NULL;

  if (a->mach == b->mach && a->mach == bfd_mach_sm83_default)
    return a;

  return NULL;
}

#define N(name,print,bits,default,next)  \
 { 16, bits, 8, bfd_arch_sm83, name, "sm83", print, 0, default, \
   compatible, bfd_default_scan, bfd_arch_default_fill, next, 0 }

const bfd_arch_info_type bfd_sm83_arch =
  N (bfd_mach_sm83_default, "sm83", 16, true, NULL);
