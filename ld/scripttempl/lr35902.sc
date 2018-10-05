# Copyright (C) 2014-2018 Free Software Foundation, Inc.
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.

cat <<EOF
/* Copyright (C) 2014-2018 Free Software Foundation, Inc.
   Copying and distribution of this script, with or without modification,
   are permitted in any medium without royalty provided the copyright
   notice and this notice are preserved.  */

/* Create a GameBoy executable. */
OUTPUT_FORMAT("binary")
OUTPUT_ARCH("lr35902")

MEMORY
{
	rom0 (rx) : ORIGIN = 0x0000, LENGTH = 16K  /* ROM bank# 0 */
	rom1 (rx) : ORIGIN = 0x4000, LENGTH = 16K  /* ROM bank# 1, 2, 3, ... */
	vram (wx) : ORIGIN = 0x8000, LENGTH =  8K  /* Display RAM */
	xram (wx) : ORIGIN = 0xA000, LENGTH =  8K  /* External Expansion Working RAM */
	wram (wx) : ORIGIN = 0xC000, LENGTH =  8K  /* Unit Working RAM */
	oam  (wx) : ORIGIN = 0xFE00, LENGTH = 160  /* Object Attribute Memory */
	hram (wx) : ORIGIN = 0xFF80, LENGTH = 127  /* Working & Stack RAM */
}

SECTIONS
{
	. = ORIGIN(rom0);

	.text : {
		FILL(0x00)

		PROVIDE(_text = .);

		. = ORIGIN(rom0) + 0x00;
		KEEP(*(.rst00))
		. = ORIGIN(rom0) + 0x08;
		KEEP(*(.rst08))
		. = ORIGIN(rom0) + 0x10;
		KEEP(*(.rst10))
		. = ORIGIN(rom0) + 0x18;
		KEEP(*(.rst18))
		. = ORIGIN(rom0) + 0x20;
		KEEP(*(.rst20))
		. = ORIGIN(rom0) + 0x28;
		KEEP(*(.rst28))
		. = ORIGIN(rom0) + 0x30;
		KEEP(*(.rst30))
		. = ORIGIN(rom0) + 0x38;
		KEEP(*(.rst38))

		. = ORIGIN(rom0) + 0x40;
		KEEP(*(.irq0))
		. = ORIGIN(rom0) + 0x48;
		KEEP(*(.irq1))
		. = ORIGIN(rom0) + 0x50;
		KEEP(*(.irq2))
		. = ORIGIN(rom0) + 0x58;
		KEEP(*(.irq3))
		. = ORIGIN(rom0) + 0x60;
		KEEP(*(.irq4))

		. = ORIGIN(rom0) + 0x100;
		PROVIDE(_entry = .);
		KEEP(*(.entry))

		. = ORIGIN(rom0) + 0x104;
		KEEP(*(.header))

		. = ORIGIN(rom0) + 0x150;
		*(.text)
		*(.text0)
		*(.bank0)
		PROVIDE(_etext = .);
	} >rom0

	. = ORIGIN(wram);

	.data : {
		PROVIDE(_data = .);
		*(.data)
		PROVIDE(_edata = .);
	} >wram AT>rom0
	PROVIDE(__load_start_data = LOADADDR(.data));
	PROVIDE(__load_stop_data = LOADADDR(.data) + SIZEOF(.data));

	.bss : {
		PROVIDE(_bstart = .);
		*(.bss)
		PROVIDE(_bend = .);
	} >wram

	. = ORIGIN(xram);

	.xram : {
		PROVIDE(_xram = .);
		*(.xram)
		PROVIDE(_exram = .);
	} >xram

	. = ORIGIN(hram);

	.hram : {
		PROVIDE(_hram = .);
		*(.hram)
		PROVIDE(_ehram = .);
	} >hram AT>rom0
	PROVIDE(__load_start_hram = LOADADDR(.hram));
	PROVIDE(__load_stop_hram = LOADADDR(.hram) + SIZEOF(.hram));

	.fill : {
		FILL(0x00)
		BYTE(0x00)
		. = ALIGN(LENGTH(rom0));
	} >rom0

	. = ORIGIN(rom1);

	OVERLAY : NOCROSSREFS AT(ORIGIN(rom1)) {
		.text1 {
			*(.text1)
			*(.bank1)
			. = ALIGN(LENGTH(rom1));
		}
		.text2 {
			*(.text2)
			*(.bank2)
			. = ALIGN(LENGTH(rom1));
		}
		.text3 {
			*(.text3)
			*(.bank3)
			. = ALIGN(LENGTH(rom1));
		}
		.text4 {
			*(.text4)
			*(.bank4)
			. = ALIGN(LENGTH(rom1));
		}
		.text5 {
			*(.text5)
			*(.bank5)
			. = ALIGN(LENGTH(rom1));
		}
		.text6 {
			*(.text6)
			*(.bank6)
			. = ALIGN(LENGTH(rom1));
		}
		.text7 {
			*(.text7)
			*(.bank7)
			. = ALIGN(LENGTH(rom1));
		}
	} >rom1
}

ENTRY(_entry)
EOF
