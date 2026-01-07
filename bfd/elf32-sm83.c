/* Sharp SM83-specific support for 32-bit ELF
   Copyright (C) 1999-2026 Free Software Foundation, Inc.
   Based on elf32-z80.c

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
#include "bfdlink.h"
#include "libbfd.h"
#include "elf-bfd.h"

#include "elf/sm83.h"

/* All users of this file have bfd_octets_per_byte (abfd, sec) == 1.  */
#define OCTETS_PER_BYTE(ABFD, SEC) 1

typedef const struct {
  bfd_reloc_code_real_type r_type;
  reloc_howto_type howto;
} bfd_howto_type;

#define BFD_EMPTY_HOWTO(rt,x) {rt, EMPTY_HOWTO(x)}
#define BFD_HOWTO(rt,a,b,c,d,e,f,g,h,i,j,k,l,m) {rt, HOWTO(a,b,c,d,e,f,g,h,i,j,k,l,m)}

static const
bfd_howto_type elf_sm83_howto_table[] =
{
  /* This reloc does nothing.  */
  BFD_HOWTO (BFD_RELOC_NONE,
	 R_SM83_NONE,		/* type */
	 0,			/* rightshift */
	 0,			/* size */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_NONE",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* A 8 bit relocation */
  BFD_HOWTO (BFD_RELOC_8,
	 R_SM83_8,		/* type */
	 0,			/* rightshift */
	 1,			/* size */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_imm8",		/* name */
	 false,			/* partial_inplace */
	 0x00,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* A 8 bit index register displacement relocation */
  BFD_HOWTO (BFD_RELOC_Z80_DISP8,
	 R_SM83_8_DIS,		/* type */
	 0,			/* rightshift */
	 1,			/* size */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_signed,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_off",		/* name */
	 false,			/* partial_inplace */
	 0x00,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* A 8 bit PC-rel relocation */
  BFD_HOWTO (BFD_RELOC_8_PCREL,
	 R_SM83_8_PCREL,	/* type */
	 0,			/* rightshift */
	 1,			/* size */
	 8,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_signed,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_jr",		/* name */
	 false,			/* partial_inplace */
	 0x00,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* An 16 bit absolute relocation */
  BFD_HOWTO (BFD_RELOC_16,
	 R_SM83_16,		/* type */
	 0,			/* rightshift */
	 2,			/* size */
	 16,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_imm16",		/* name */
	 false,			/* partial_inplace */
	 0x00000000,		/* src_mask */
	 0x0000ffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* A 24 bit absolute relocation emitted by ADL mode operands */
  BFD_HOWTO (BFD_RELOC_24,
	 R_SM83_24,		/* type */
	 0,			/* rightshift */
	 3,			/* size */
	 24,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_imm24",		/* name */
	 false,			/* partial_inplace */
	 0x00000000,		/* src_mask */
	 0x00ffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  BFD_HOWTO (BFD_RELOC_32,
	 R_SM83_32,		/* type */
	 0,			/* rightshift */
	 4,			/* size */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_imm32",		/* name */
	 false,			/* partial_inplace */
	 0x00000000,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* First (lowest) 8 bits of multibyte relocation */
  BFD_HOWTO (BFD_RELOC_Z80_BYTE0,
	 R_SM83_BYTE0,		/* type */
	 0,			/* rightshift */
	 1,			/* size */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_byte0",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* Second 8 bits of multibyte relocation */
  BFD_HOWTO (BFD_RELOC_Z80_BYTE1,
	 R_SM83_BYTE1,		/* type */
	 8,			/* rightshift */
	 1,			/* size */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_byte1",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* Third 8 bits of multibyte relocation */
  BFD_HOWTO (BFD_RELOC_Z80_BYTE2,
	 R_SM83_BYTE2,		/* type */
	 16,			/* rightshift */
	 1,			/* size */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_byte2",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* Fourth (highest) 8 bits of multibyte relocation */
  BFD_HOWTO (BFD_RELOC_Z80_BYTE3,
	 R_SM83_BYTE3,		/* type */
	 24,			/* rightshift */
	 1,			/* size */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_byte3",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* An 16 bit absolute relocation of lower word of multibyte value */
  BFD_HOWTO (BFD_RELOC_Z80_WORD0,
	 R_SM83_WORD0,		/* type */
	 0,			/* rightshift */
	 2,			/* size */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_word0",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* An 16 bit absolute relocation of higher word of multibyte value */
  BFD_HOWTO (BFD_RELOC_Z80_WORD1,
	 R_SM83_WORD1,		/* type */
	 16,			/* rightshift */
	 2,			/* size */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_word1",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffff,		/* dst_mask */
	 false),		/* pcrel_offset */
};

static reloc_howto_type *
sm83_reloc_type_lookup (bfd *abfd ATTRIBUTE_UNUSED,
			bfd_reloc_code_real_type code)
{
  enum
    {
      table_size = sizeof (elf_sm83_howto_table) / sizeof (elf_sm83_howto_table[0])
    };
  unsigned int i;

  for (i = 0; i < table_size; i++)
    {
      if (elf_sm83_howto_table[i].r_type == code)
	  return &elf_sm83_howto_table[i].howto;
    }

  printf ("%s:%d Not found BFD reloc type %d\n", __FILE__, __LINE__, code);

  return NULL;
}

static reloc_howto_type *
sm83_reloc_name_lookup (bfd *abfd ATTRIBUTE_UNUSED, const char *r_name)
{
  enum
    {
      table_size = sizeof (elf_sm83_howto_table) / sizeof (elf_sm83_howto_table[0])
    };
  unsigned int i;

  for (i = 0; i < table_size; i++)
    {
      if (elf_sm83_howto_table[i].howto.name != NULL
	  && strcasecmp (elf_sm83_howto_table[i].howto.name, r_name) == 0)
	return &elf_sm83_howto_table[i].howto;
    }

  printf ("%s:%d Not found ELF reloc name `%s'\n", __FILE__, __LINE__, r_name);

  return NULL;
}

static reloc_howto_type *
sm83_rtype_to_howto (bfd *abfd, unsigned r_type)
{
  enum
    {
      table_size = sizeof (elf_sm83_howto_table) / sizeof (elf_sm83_howto_table[0])
    };
  unsigned int i;

  for (i = 0; i < table_size; i++)
    {
      if (elf_sm83_howto_table[i].howto.type == r_type)
	  return &elf_sm83_howto_table[i].howto;
    }

  /* xgettext:c-format */
  _bfd_error_handler (_("%pB: unsupported relocation type %#x"),
		      abfd, r_type);
  return NULL;
} 

/* Set the howto pointer for an SM83 ELF reloc.  */

static bool
sm83_info_to_howto_rela (bfd *abfd, arelent *cache_ptr, Elf_Internal_Rela *dst)
{
  unsigned int  r_type = ELF32_R_TYPE (dst->r_info);
  reloc_howto_type *howto = sm83_rtype_to_howto (abfd, r_type);
  if (howto != NULL)
    {
      cache_ptr->howto = howto;
      return true;
    }
  bfd_set_error (bfd_error_bad_value);
  return false;
}

static bfd_reloc_status_type
sm83_elf_final_link_relocate (unsigned long r_type,
			      bfd *input_bfd,
			      bfd *output_bfd ATTRIBUTE_UNUSED,
			      asection *input_section ATTRIBUTE_UNUSED,
			      bfd_byte *contents,
			      bfd_vma offset,
			      bfd_vma value,
			      bfd_vma addend,
			      struct bfd_link_info *info ATTRIBUTE_UNUSED,
			      asection *sym_sec ATTRIBUTE_UNUSED,
			      int is_local ATTRIBUTE_UNUSED)
{
  bool r;
  reloc_howto_type *howto;

  howto = sm83_rtype_to_howto (input_bfd, r_type);
  if (howto == NULL)
    return bfd_reloc_notsupported;

  r = _bfd_final_link_relocate (howto, input_bfd, input_section, contents,
				offset, value, addend);
  return r ? bfd_reloc_ok : bfd_reloc_notsupported;
}

static int
sm83_elf_relocate_section (bfd *output_bfd,
			   struct bfd_link_info *info,
			   bfd *input_bfd,
			   asection *input_section,
			   bfd_byte *contents,
			   Elf_Internal_Rela *relocs,
			   Elf_Internal_Sym *local_syms,
			   asection **local_sections)
{
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  Elf_Internal_Rela *rel, *relend;

  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (input_bfd);

  rel = relocs;
  relend = relocs + input_section->reloc_count;
  for (; rel < relend; rel++)
    {
      unsigned int r_type;
      unsigned long r_symndx;
      Elf_Internal_Sym *sym;
      asection *sec;
      struct elf_link_hash_entry *h;
      bfd_vma relocation;

      /* This is a final link.  */
      r_symndx = ELF32_R_SYM (rel->r_info);
      r_type = ELF32_R_TYPE (rel->r_info);
      h = NULL;
      sym = NULL;
      sec = NULL;
      if (r_symndx < symtab_hdr->sh_info)
	{
	  sym = local_syms + r_symndx;
	  sec = local_sections[r_symndx];
	  relocation = _bfd_elf_rela_local_sym (output_bfd, sym, &sec, rel);
	}
      else
	{
	  bool unresolved_reloc, warned, ignored;

	  RELOC_FOR_GLOBAL_SYMBOL (info, input_bfd, input_section, rel,
				   r_symndx, symtab_hdr, sym_hashes,
				   h, sec, relocation,
				   unresolved_reloc, warned, ignored);
	}

      if (sec != NULL && discarded_section (sec))
	{
	  /* For relocs against symbols from removed linkonce sections,
	     or sections discarded by a linker script, we just want the
	     section contents cleared.  Avoid any special processing.  */
	  reloc_howto_type *howto;
	  howto = sm83_rtype_to_howto (input_bfd, r_type);
	  RELOC_AGAINST_DISCARDED_SECTION (info, input_bfd, input_section,
					   rel, 1, relend, howto, 0, contents);
	}

      if (bfd_link_relocatable (info))
	continue;


      sm83_elf_final_link_relocate (r_type, input_bfd, output_bfd,
				    input_section,
				    contents, rel->r_offset,
				    relocation, rel->r_addend,
				    info, sec, h == NULL);
    }

  return true;
}

/* The final processing done just before writing out a SM83 ELF object
   file.  */

static bool
sm83_elf_final_write_processing (bfd *abfd)
{
  elf_elfheader (abfd)->e_machine = EM_SM83;
  elf_elfheader (abfd)->e_flags &= ~EF_SM83_MACH_MSK;
  elf_elfheader (abfd)->e_flags |= EF_SM83_MACH_DEFAULT;
  return _bfd_elf_final_write_processing (abfd);
}

/* Set the right machine number.  */
static bool
sm83_elf_object_p (bfd *abfd)
{
  if (elf_elfheader (abfd)->e_machine == EM_SM83)
    {
      int e_mach = elf_elfheader (abfd)->e_flags & EF_SM83_MACH_MSK;
      switch (e_mach)
	{
	default:
	  _bfd_error_handler (_("%pB: unsupported mach %#x"),
			      abfd, e_mach);
	  /* fall through */
	case EF_SM83_MACH_DEFAULT:
	  break;
	}
    }
  else
    {
      _bfd_error_handler (_("%pB: unsupported arch %#x"),
			  abfd, elf_elfheader (abfd)->e_machine);
    }
  return bfd_default_set_arch_mach (abfd, bfd_arch_sm83, bfd_mach_sm83_default);
}

static bool
sm83_is_local_label_name (bfd *abfd ATTRIBUTE_UNUSED,
			  const char *name)
{
  return (name[0] == '.' && name[1] == 'L') ||
	 _bfd_elf_is_local_label_name (abfd, name);
}

#define ELF_ARCH		bfd_arch_sm83
#define ELF_MACHINE_CODE	EM_SM83
#define ELF_MAXPAGESIZE		0x10000

#define TARGET_LITTLE_SYM		sm83_elf32_vec
#define TARGET_LITTLE_NAME		"elf32-sm83"

#define elf_backend_can_refcount		1
#define elf_backend_can_gc_sections		1
#define elf_backend_stack_align			1
#define elf_backend_rela_normal			1

#define elf_info_to_howto			sm83_info_to_howto_rela
#define elf_info_to_howto_rel			sm83_info_to_howto_rela

#define elf_backend_final_write_processing	sm83_elf_final_write_processing
#define elf_backend_object_p			sm83_elf_object_p
#define elf_backend_relocate_section		sm83_elf_relocate_section

#define bfd_elf32_bfd_reloc_type_lookup		sm83_reloc_type_lookup
#define bfd_elf32_bfd_reloc_name_lookup		sm83_reloc_name_lookup
#define bfd_elf32_bfd_is_local_label_name	sm83_is_local_label_name

#include "elf32-target.h"
