# Copyright (C) 2014-2018 Free Software Foundation, Inc.
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.

LAST_BANK=511

cat <<"EOF"
/* Copyright (C) 2014-2018 Free Software Foundation, Inc.
   Copying and distribution of this script, with or without modification,
   are permitted in any medium without royalty provided the copyright
   notice and this notice are preserved.  */

/* NOTE:
 * If bank1 and one additional switchable bank are the only two switchable
 * banks being used, and there is data to be loaded from .text1 to .data or
 * .hdata, then you may encounter a linker error like this:
 *   sm83-gb-coff-ld: section .text1 VMA [0000000000004020,0000000000007fff]
 *   overlaps section .text2 VMA [0000000000004000,0000000000007fff]
 * In this case, the starting VMA of .text1 and .text2 are different, but
 * the sections overlap. The linker will complain about this overlap, but
 * only if there are no overlay sections with exactly the same starting VMA.
 * Having two or more sections (with actual content, so they won't get dropped)
 * with the same starting VMA, the linker will stop throwing errors for
 * overlapping sections, because then it thinks overlaps are intentional.
 * This means you can fix this error just by using one more bank.
 * See binutils 2.31.1 source file ld/ldlang.c; lines 4890 and 4899. */

/* Create a GameBoy executable. */
OUTPUT_FORMAT("binary")
OUTPUT_ARCH("sm83")

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
	.text ORIGIN(rom0) : AT(ORIGIN(rom0)) {
		PROVIDE(_text = .);

		FILL(0x00)

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
		KEEP(*(.hdrlogo))              /* "Nintendo" character data */
		. = ORIGIN(rom0) + 0x134;
		KEEP(*(.hdrname))              /* Game Title (max. 11 bytes) */
		. = ORIGIN(rom0) + 0x13F;
		KEEP(*(.hdrgc))                /* Game Code (4 bytes) */
		. = ORIGIN(rom0) + 0x143;
		KEEP(*(.hdrcgb))               /* CGB Support Code */
		. = ORIGIN(rom0) + 0x144;
		KEEP(*(.hdrmc))                /* Maker Code (2 bytes) */
		. = ORIGIN(rom0) + 0x146;
		KEEP(*(.hdrsgb))               /* SGB Support Code */
		. = ORIGIN(rom0) + 0x147;
		KEEP(*(.hdrtype))              /* Cartridge Type */
		. = ORIGIN(rom0) + 0x148;
		KEEP(*(.hdrrom))               /* ROM Size */
		. = ORIGIN(rom0) + 0x149;
		KEEP(*(.hdrram))               /* External RAM Size */
		. = ORIGIN(rom0) + 0x14A;
		KEEP(*(.hdrdest))              /* Destination Code */
		. = ORIGIN(rom0) + 0x14B;
		BYTE(0x33)
		. = ORIGIN(rom0) + 0x14C;
		KEEP(*(.hdrver))               /* ROM Version */
		. = ORIGIN(rom0) + 0x14D;
		BYTE(0x00)                     /* Complement Check (1 byte)                 */
		BYTE(0x00)                     /* Check Sum (2 bytes)                       */
		BYTE(0x00)                     /* Run 'rgbfix -fhg' to patch after linking. */

		. = ORIGIN(rom0) + 0x150;
		*(.text .text.*)
		*(.text0 .text0.*)
		*(.bank0 .bank0.*)
		*(.rodata .rodata.*)
		PROVIDE(_etext = .);
		ASSERT(. <= (ORIGIN(rom0) + LENGTH(rom0)), "Error: .text0 section doesn't fit into rom0 region");
	}

	. = ORIGIN(hram);
	.hdata ORIGIN(hram) : AT(ORIGIN(rom1)) {
		PROVIDE(_hdata = .);
		*(.hdata .hdata.*)
		PROVIDE(_ehdata = .);
		ASSERT(. <= (ORIGIN(hram) + LENGTH(hram)), "Error: .hdata section doesn't fit into hram region");
	}
	PROVIDE(__load_start_hdata = LOADADDR(.hdata));
	PROVIDE(__load_stop_hdata = LOADADDR(.hdata) + SIZEOF(.hdata));
	PROVIDE(__size_hdata = SIZEOF(.hdata));

	.hbss (ORIGIN(hram) + SIZEOF(.hdata)) (NOLOAD) : {
		PROVIDE(_hbstart = .);
		*(.hbss .hbss.*)
		PROVIDE(_hbend = .);
		ASSERT(. <= (ORIGIN(hram) + LENGTH(hram)), "Error: .hbss section doesn't fit into hram region");
		PROVIDE(_initial_sp = ORIGIN(hram) + LENGTH(hram) - 1);
	}
	PROVIDE(__size_hbss = SIZEOF(.hbss));

	. = ORIGIN(wram);
	.data ORIGIN(wram) : AT(ORIGIN(rom1) + SIZEOF(.hdata)) {
		PROVIDE(_data = .);
		*(.data .data.*)
		PROVIDE(_edata = .);
		ASSERT(. <= (ORIGIN(wram) + LENGTH(wram)), "Error: .data section doesn't fit into wram region");
	}
	PROVIDE(__load_start_data = LOADADDR(.data));
	PROVIDE(__load_stop_data = LOADADDR(.data) + SIZEOF(.data));
	PROVIDE(__size_data = SIZEOF(.data));

	.bss (ORIGIN(wram) + SIZEOF(.data)) (NOLOAD) : {
		PROVIDE(_bstart = .);
		*(COMMON)
		*(.bss .bss.*)
		PROVIDE(_bend = .);
		ASSERT(. <= (ORIGIN(wram) + LENGTH(wram)), "Error: .bss section doesn't fit into wram region");
	}
	PROVIDE(__size_bss = SIZEOF(.bss));

	. = ORIGIN(xram);
	.xram ORIGIN(xram) (NOLOAD) : {
		PROVIDE(_xram = .);
		*(.xram .xram.*)
		*(.nvram .nvram.*)
		PROVIDE(_exram = .);
		ASSERT(. <= (ORIGIN(xram) + LENGTH(xram)), "Error: .xram section doesn't fit into xram region");
	}
	PROVIDE(__size_xram = SIZEOF(.xram));

	. = ORIGIN(rom1) + SIZEOF(.hdata) + SIZEOF(.data);
	.text1 ORIGIN(rom1) + SIZEOF(.hdata) + SIZEOF(.data) : AT(ORIGIN(rom1) + SIZEOF(.hdata) + SIZEOF(.data)) {
		PROVIDE(_text1 = .);
		*(.text1 .text1.*)
		*(.bank1 .bank1.*)
		PROVIDE(_etext1 = .);

		/* HACK: Make sure this section has at least one byte of content, so it won't
		 *       get dropped. This ensures that the output file always has the minimum
		 *       size of 32 kbyte. Remove this line if you really need one extra byte. */
		BYTE(0x00)

		ASSERT(. <= (ORIGIN(rom1) + LENGTH(rom1)), "Error: .text1 section doesn't fit into rom1 region");
		. = ALIGN(LENGTH(rom1));
	}
EOF

for i in `seq 2 $LAST_BANK`
do
	let 'j=i-1'
	echo "	. = ORIGIN(rom1);"
	echo "	.text$i ORIGIN(rom1) : AT(ORIGIN(rom1) + LENGTH(rom1) * $j) {"
	echo "		PROVIDE(_text$i = .);"
	echo "		*(.text$i .text$i.*)"
	echo "		*(.bank$i .bank$i.*)"
	echo "		PROVIDE(_etext$i = .);"
	echo "		ASSERT(. <= (ORIGIN(rom1) + LENGTH(rom1)), \"Error: .text$i section doesn't fit into rom1 region\");"
	echo "		. = ALIGN(LENGTH(rom1));"
	echo "	}"
done

cat <<"EOF"
}

/* Only one of the switchable banks can be mapped to 0x4000 at
 * any given time, so cross references between those banks are
 * very likely some kind of error. */
EOF

echo -n "NOCROSSREFS(.text1"
for i in `seq 2 $LAST_BANK`
do
	let 'j=i&7'
	echo -n " .text$i"
	if [ $j -eq 7 ]
	then
		echo
	fi
done
echo ")"

cat <<"EOF"

ENTRY(_entry)
EOF
