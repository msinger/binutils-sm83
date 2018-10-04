/* BFD library support routines for the Sharp LR35902 CPU.
   Copyright (C) 2005-2018 Free Software Foundation, Inc.

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

const bfd_arch_info_type bfd_lr35902_arch =
  {
    16,                     /* word size */
    16,                     /* ptr size */
    8,                      /* byte size */
    bfd_arch_lr35902,       /* enum bfd_architecture arch */
    NULL,                   /* Machine value, used to distinguish between variants */
    "lr35902",              /* Architecture name (short version) */
    "lr35902",              /* Architecture name (long version) */
    0,                      /* Section alignment power */
    TRUE,                   /* True if this is the default machine for the architecture */
    bfd_default_compatible, /* Function to call to determine if two different architectures are compatible */
    bfd_default_scan,       /* Function to call to determine if a given string matches this architecture */
    bfd_arch_default_fill,
    NULL                    /* Pointer to the next CR16 machine architecture */
  };
