/* tc-sm83.c -- Assemble code for the Sharp SM83 (Game Boy)
   Copyright (C) 2005-2026 Free Software Foundation, Inc.
   Based on config/tc-z80.c

   This file is part of GAS, the GNU Assembler.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GAS is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GAS; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

#include "as.h"
#include "safe-ctype.h"
#include "subsegs.h"
#include "elf/sm83.h"
#include "dwarf2dbg.h"
#include "dw2gencfi.h"

/* Exported constants.  */
const char comment_chars[] = ";";
const char line_comment_chars[] = "#;";
const char line_separator_chars[] = "";
const char EXP_CHARS[] = "eE";
const char FLT_CHARS[] = "RrDdFfSsHh";

/* For machine specific options.  */
const char md_shortopts[] = ""; /* None yet.  */

enum options
{
  OPTION_FP_SINGLE_FORMAT = OPTION_MD_BASE,
  OPTION_FP_DOUBLE_FORMAT,
  OPTION_COMPAT_LL_PREFIX,
  OPTION_COMPAT_COLONLESS,
  OPTION_COMPAT_SDCC
};

const struct option md_longopts[] =
{
  { "fp-s",         required_argument, NULL, OPTION_FP_SINGLE_FORMAT },
  { "fp-d",         required_argument, NULL, OPTION_FP_DOUBLE_FORMAT },
  { "local-prefix", required_argument, NULL, OPTION_COMPAT_LL_PREFIX },
  { "colonless",    no_argument,       NULL, OPTION_COMPAT_COLONLESS },
  { "sdcc",         no_argument,       NULL, OPTION_COMPAT_SDCC },

  { NULL, no_argument, NULL, 0 }
};

const size_t md_longopts_size = sizeof (md_longopts);

/* accept SDCC specific instruction encoding */
static int sdcc_compat = 0;
/* accept colonless labels */
static int colonless_labels = 0;
/* local label prefix (NULL - default) */
static const char *local_label_prefix = NULL;
/* floating point support */
typedef const char *(*str_to_float_t)(char *litP, int *sizeP);
static str_to_float_t str_to_float;
static str_to_float_t str_to_double;

static int signed_overflow (signed long value, unsigned bitsize);
static int unsigned_overflow (unsigned long value, unsigned bitsize);
static int is_overflow (long value, unsigned bitsize);

static const char *
str_to_zeda32 (char *litP, int *sizeP);
static const char *
str_to_float48 (char *litP, int *sizeP);
static const char *
str_to_ieee754_h (char *litP, int *sizeP);
static const char *
str_to_ieee754_s (char *litP, int *sizeP);
static const char *
str_to_ieee754_d (char *litP, int *sizeP);

static str_to_float_t
get_str_to_float (const char *arg)
{
  if (strcasecmp (arg, "zeda32") == 0)
    return str_to_zeda32;

  if (strcasecmp (arg, "math48") == 0)
    return str_to_float48;

  if (strcasecmp (arg, "half") != 0)
    return str_to_ieee754_h;

  if (strcasecmp (arg, "single") != 0)
    return str_to_ieee754_s;

  if (strcasecmp (arg, "double") != 0)
    return str_to_ieee754_d;

  if (strcasecmp (arg, "ieee754") == 0)
    as_fatal (_("invalid floating point numbers type `%s'"), arg);
  return NULL;
}

int
md_parse_option (int c, const char *arg)
{
  switch (c)
    {
    default:
      return 0;
    case OPTION_FP_SINGLE_FORMAT:
      str_to_float = get_str_to_float (arg);
      break;
    case OPTION_FP_DOUBLE_FORMAT:
      str_to_double = get_str_to_float (arg);
      break;
    case OPTION_COMPAT_LL_PREFIX:
      local_label_prefix = (arg && *arg) ? arg : NULL;
      break;
    case OPTION_COMPAT_SDCC:
      sdcc_compat = 1;
      break;
    case OPTION_COMPAT_COLONLESS:
      colonless_labels = 1;
      break;
    }

  return 1;
}

void
md_show_usage (FILE *f)
{
  fprintf (f, _("\n\
Compatibility options:\n\
  -local-prefix=TEXT\t  treat labels prefixed by TEXT as local\n\
  -colonless\t\t  permit colonless labels\n\
  -sdcc\t\t\t  accept SDCC specific instruction syntax\n\
  -fp-s=FORMAT\t\t  set single precision FP numbers format\n\
  -fp-d=FORMAT\t\t  set double precision FP numbers format\n\
Where FORMAT one of:\n\
  ieee754\t\t  IEEE754 compatible (depends on directive)\n\
  half\t\t\t  IEEE754 half precision (16 bit)\n\
  single\t\t  IEEE754 single precision (32 bit)\n\
  double\t\t  IEEE754 double precision (64 bit)\n\
  zeda32\t\t  Zeda z80float library 32 bit format\n\
  math48\t\t  48 bit format from Math48 library\n"));
}

static symbolS *zero;

struct reg_entry
{
  const char* name;
  int number;
};

#define R_STACKABLE   (0x80)
#define R_ARITH       (0x40)
#define R_POST_INCDEC (0x20)

#define REG_A (7)
#define REG_B (0)
#define REG_C (1)
#define REG_D (2)
#define REG_E (3)
#define REG_H (4)
#define REG_L (5)

#define REG_AF  (3    | R_STACKABLE)
#define REG_BC  (0    | R_STACKABLE | R_ARITH)
#define REG_DE  (1    | R_STACKABLE | R_ARITH)
#define REG_HL  (2    | R_STACKABLE | R_ARITH)
#define REG_SP  (3    | R_ARITH)
#define REG_HLI (2    | R_POST_INCDEC)
#define REG_HLD (0x12 | R_POST_INCDEC)

static const struct reg_entry regtable[] =
{
  {"a",   REG_A   },
  {"af",  REG_AF  },
  {"b",   REG_B   },
  {"bc",  REG_BC  },
  {"c",   REG_C   },
  {"d",   REG_D   },
  {"de",  REG_DE  },
  {"e",   REG_E   },
  {"h",   REG_H   },
  {"hl",  REG_HL  },
  {"hld", REG_HLD },
  {"hli", REG_HLI },
  {"l",   REG_L   },
  {"sp",  REG_SP  },
};

#define BUFLEN 8 /* Large enough for any keyword.  */

void
md_begin (void)
{
  expressionS nul, reg;
  char * p;
  unsigned int i, j, k;
  char buf[BUFLEN];

  memset (&reg, 0, sizeof (reg));
  memset (&nul, 0, sizeof (nul));

  reg.X_op = O_register;
  reg.X_md = 0;
  reg.X_add_symbol = reg.X_op_symbol = 0;
  for ( i = 0 ; i < ARRAY_SIZE ( regtable ) ; ++i )
    {
      reg.X_add_number = regtable[i].number;
      k = strlen ( regtable[i].name );
      buf[k] = 0;
      if ( k+1 < BUFLEN )
	{
	  for ( j = ( 1<<k ) ; j ; --j )
	    {
	      for ( k = 0 ; regtable[i].name[k] ; ++k )
		{
		  buf[k] = ( j & ( 1<<k ) ) ? TOUPPER (regtable[i].name[k]) : regtable[i].name[k];
		}
	      symbolS * psym = symbol_find_or_make (buf);
	      S_SET_SEGMENT (psym, reg_section);
	      symbol_set_value_expression (psym, &reg);
	    }
	}
    }
  p = input_line_pointer;
  input_line_pointer = (char *) "0";
  nul.X_md=0;
  expression (& nul);
  input_line_pointer = p;
  zero = make_expr_symbol (& nul);
  /* We do not use relaxation (yet).  */
  linkrelax = 0;
}

void
sm83_md_finish (void)
{
  bfd_set_arch_mach (stdoutput, TARGET_ARCH, bfd_mach_sm83_default);
}

#if defined (OBJ_ELF) || defined (OBJ_MAYBE_ELF)
void
sm83_elf_final_processing (void)
{
}
#endif

static const char *
skip_space (const char *s)
{
  while (is_whitespace (*s))
    ++s;
  return s;
}

/* A non-zero return-value causes a continue in the
   function read_a_source_file () in ../read.c.  */
int
sm83_start_line_hook (void)
{
  char *p, quote;
  char buf[4];

  /* Convert one character constants.  */
  for (p = input_line_pointer; *p && *p != '\n'; ++p)
    {
      switch (*p)
	{
	case '\'':
	  if (p[1] != 0 && p[1] != '\'' && p[2] == '\'')
	    {
	      snprintf (buf, 4, "%3d", (unsigned char)p[1]);
	      *p++ = buf[0];
	      *p++ = buf[1];
	      *p++ = buf[2];
	      break;
	    }
	  /* Fall through.  */
	case '"':
	  for (quote = *p++; quote != *p && '\n' != *p; ++p)
	    /* No escapes.  */ ;
	  if (quote != *p)
	    {
	      as_bad (_("-- unterminated string"));
	      ignore_rest_of_line ();
	      return 1;
	    }
	  break;
	case '#': /* force to use next expression as immediate value in SDCC */
	  if (!sdcc_compat)
	   break;
	  if (is_whitespace (p[1]) && *skip_space (p + 1) == '(')
	    { /* ld a,# (expr)... -> ld a,0+(expr)... */
	      *p++ = '0';
	      *p = '+';
	    }
	  else /* ld a,#(expr)... -> ld a,+(expr); ld a,#expr -> ld a, expr */
	    *p = (p[1] == '(') ? '+' : ' ';
	  break;
	}
    }
  /* Remove leading zeros from dollar local labels if SDCC compat enabled.  */
  if (sdcc_compat && *input_line_pointer == '0')
    {
      char *dollar;

      /* SDCC emits at most one label definition per line, so it is
	 enough to look at only the first label.  Hand-written asm
	 might use more, but then it is unlikely to use leading zeros
	 on dollar local labels.  */

      /* Place p at the first character after [0-9]+.  */
      for (p = input_line_pointer; *p >= '0' && *p <= '9'; ++p)
	;

      /* Is this a dollar sign label?
	 GAS allows spaces between $ and :, but SDCC does not.  */
      if (p[0] == '$' && p[1] == ':')
	{
	  dollar = p;
	  /* Replace zeros with spaces until the first non-zero,
	     but leave the last character before $ intact (for e.g. 0$:).  */
	  for (p = input_line_pointer; *p == '0' && p < dollar - 1; ++p)
	    {
	      *p = ' ';
	    }
	}
    }
  /* Check for <label>[:] =|([.](EQU|DEFL)) <value>.  */
  if (is_name_beginner (*input_line_pointer))
    {
      char *name;
      char c, *rest, *line_start;
      int len;

      line_start = input_line_pointer;
      if (ignore_input ())
	return 0;
      c = get_symbol_name (&name);
      rest = input_line_pointer + 1;
      if (c == ':' && *rest == ':')
        {
          /* remove second colon if SDCC compatibility enabled */
          if (sdcc_compat)
            *rest = ' ';
          ++rest;
        }
      rest = (char*)skip_space (rest);
      if (*rest == '=')
	len = (rest[1] == '=') ? 2 : 1;
      else
	{
	  if (*rest == '.')
	    ++rest;
	  if (strncasecmp (rest, "EQU", 3) == 0)
	    len = 3;
	  else if (strncasecmp (rest, "DEFL", 4) == 0)
	    len = 4;
	  else
	    len = 0;
	}
      if (len && (len <= 2 || !ISALPHA (rest[len])))
	{
	  /* Handle assignment here.  */
	  if (line_start[-1] == '\n')
	    {
	      bump_line_counters ();
	      LISTING_NEWLINE ();
	    }
	  input_line_pointer = rest + len - 1;
	  /* Allow redefining with "DEFL" (len == 4), but not with "EQU".  */
	  switch (len)
	    {
	    case 1: /* label = expr */
	    case 4: /* label DEFL expr */
	      equals (name, 1);
	      break;
	    case 2: /* label == expr */
	    case 3: /* label EQU expr */
	      equals (name, 0);
	      break;
	    }
	  return 1;
	}
      else
	{
	  /* Restore line and pointer.  */
	  (void) restore_line_pointer (c);
	  input_line_pointer = line_start;
	}
    }
  return 0;
}

symbolS *
md_undefined_symbol (char *name ATTRIBUTE_UNUSED)
{
  return NULL;
}

const char *
md_atof (int type, char *litP, int *sizeP)
{
  switch (type)
    {
    case 'f':
    case 'F':
    case 's':
    case 'S':
      if (str_to_float)
	return str_to_float (litP, sizeP);
      break;
    case 'd':
    case 'D':
    case 'r':
    case 'R':
      if (str_to_double)
	return str_to_double (litP, sizeP);
      break;
    }
  return ieee_md_atof (type, litP, sizeP, false);
}

valueT
md_section_align (segT seg ATTRIBUTE_UNUSED, valueT size)
{
  return size;
}

long
md_pcrel_from (fixS * fixp)
{
  return fixp->fx_where + fixp->fx_frag->fr_address;
}

typedef const char * (asfunc)(char, char, const char*);

typedef struct _table_t
{
  const char    *name;
  unsigned char prefix;
  unsigned char opcode;
  asfunc        *fp;
} table_t;

/* Compares the key for structs that start with a char * to the key.  */
static int
key_cmp (const void *a, const void *b)
{
  const char *str_a, *str_b;

  str_a = *((const char**)a);
  str_b = *((const char**)b);
  return strcmp (str_a, str_b);
}

char buf[BUFLEN];
const char *key = buf;

/* Prevent an error on a line from also generating
   a "junk at end of line" error message.  */
static char err_flag;

static void
error (const char *message)
{
  if (err_flag)
    return;

  as_bad ("%s", message);
  err_flag = 1;
}

static void
ill_op (void)
{
  error (_("illegal operand"));
}

/* Check whether an expression is indirect.  */
static int
is_indir (const char *s)
{
  char quote;
  const char *p;
  int indir, depth;

  /* Indirection is indicated with parentheses.  */
  indir = (*s == '(');

  for (p = s, depth = 0; *p && *p != ','; ++p)
    {
      switch (*p)
	{
	case '"':
	case '\'':
	  for (quote = *p++; quote != *p && *p != '\n'; ++p)
	    if (*p == '\\' && p[1])
	      ++p;
	  break;
	case '(':
	  ++ depth;
	  break;
	case ')':
	  -- depth;
	  if (depth == 0)
	    {
	      p = skip_space (p + 1);
	      if (*p && *p != ',')
		indir = 0;
	      --p;
	    }
	  if (depth < 0)
	    error (_("mismatched parentheses"));
	  break;
	}
    }

  if (depth != 0)
    error (_("mismatched parentheses"));

  return indir;
}

/* Check whether a symbol involves a register.  */
static bool
contains_register (symbolS *sym)
{
  if (sym)
    {
      expressionS *ex = symbol_get_value_expression (sym);

      if (!ex)
	return false;

      switch (ex->X_op)
	{
	case O_register:
	  return true;

	case O_add:
	case O_subtract:
	  if (ex->X_op_symbol && contains_register (ex->X_op_symbol))
	    return true;
	  /* Fall through.  */
	case O_uminus:
	case O_symbol:
	  if (ex->X_add_symbol && contains_register (ex->X_add_symbol))
	    return true;
	  break;

	default:
	  break;
	}
    }

  return false;
}

/* Parse general expression, not looking for indexed addressing.  */
static const char *
parse_exp_not_indexed (const char *s, expressionS *op)
{
  const char *p;
  int indir;
  int make_shift = -1;

  memset (op, 0, sizeof (*op));
  p = skip_space (s);
  if (sdcc_compat && (*p == '<' || *p == '>'))
    {
      switch (*p)
	{
	case '<': /* LSB request */
	  make_shift = 0;
	  break;
	case '>': /* MSB request */
	  make_shift = 8;
	  break;
	}
      s = ++p;
      p = skip_space (p);
    }

  if (make_shift == -1)
    indir = is_indir (p);
  else
    indir = 0;
  op->X_md = indir;
  if (indir)
    { /* check for instructions like ld a,(hl+), ld (hl-),a */
      p = skip_space (p+1);
      if (!strncasecmp (p, "hl", 2))
	{
	  p = skip_space(p+2);
	  if (*skip_space(p+1) == ')' && (*p == '+' || *p == '-'))
	    {
	      op->X_op = O_register;
	      op->X_add_symbol = NULL;
	      op->X_add_number = (*p == '+') ? REG_HLI : REG_HLD;
	      input_line_pointer = (char*)skip_space(p + 1) + 1;
	      return input_line_pointer;
	    }
	}
    }
  input_line_pointer = (char*)s;
  expression (op);
  resolve_register (op);
  switch (op->X_op)
    {
    case O_absent:
      error (_("missing operand"));
      break;
    case O_illegal:
      error (_("bad expression syntax"));
      break;
    default:
      break;
    }

  if (make_shift >= 0)
    {
      /* replace [op] by [op >> shift] */
      expressionS data;
      op->X_add_symbol = make_expr_symbol (op);
      op->X_add_number = 0;
      op->X_op = O_right_shift;
      memset (&data, 0, sizeof (data));
      data.X_op = O_constant;
      data.X_add_number = make_shift;
      op->X_op_symbol = make_expr_symbol (&data);
    }
  return input_line_pointer;
}

static int
unify_indexed (expressionS *op)
{
  if (!op->X_add_symbol)
    return 0;

  expressionS *lhs = symbol_get_value_expression (op->X_add_symbol);
  if (!lhs || lhs->X_op != O_register)
    return 0;

  int rnum = symbol_get_value_expression (op->X_add_symbol)->X_add_number;

  if (contains_register (op->X_op_symbol))
    {
      ill_op ();
      return 0;
    }

  /* Convert subtraction to addition of negative value.  */
  if (O_subtract == op->X_op)
    {
      expressionS minus;
      memset (&minus, 0, sizeof (minus));
      minus.X_op = O_uminus;
      minus.X_add_symbol = op->X_op_symbol;
      op->X_op_symbol = make_expr_symbol (&minus);
      op->X_op = O_add;
    }

  /* Clear X_add_number of the expression.  */
  if (op->X_add_number != 0)
    {
      expressionS add;
      memset (&add, 0, sizeof (add));
      add.X_op = O_symbol;
      add.X_add_number = op->X_add_number;
      add.X_add_symbol = op->X_op_symbol;
      op->X_add_symbol = make_expr_symbol (&add);
    }
  else
    op->X_add_symbol = op->X_op_symbol;

  op->X_add_number = rnum;
  op->X_op_symbol = 0;
  return 1;
}

/* Parse expression, change operator to O_md1 for indexed addressing.  */
static const char *
parse_exp (const char *s, expressionS *op)
{
  const char* res = parse_exp_not_indexed (s, op);
  switch (op->X_op)
    {
    case O_add:
    case O_subtract:
      if (unify_indexed (op) && op->X_md)
        op->X_op = O_md1;
      break;
    case O_constant:
      /* parse SDCC syntax where index register offset placed before parentheses */
      if (sdcc_compat && is_indir (res))
	{
	  expressionS off;
	  off = *op;
	  res = parse_exp (res, op);
	  if (op->X_op != O_md1 || op->X_add_symbol != zero)
	    ill_op ();
	  else
	    op->X_add_symbol = make_expr_symbol (&off);
	}
      break;
    default:
      break;
    }
  return res;
}

/* Condition codes, including some synonyms provided by HiTech zas.  */
static const struct reg_entry cc_tab[] =
{
  { "c",   3 << 3 },
  { "lge", 2 << 3 },
  { "llt", 3 << 3 },
  { "nc",  2 << 3 },
  { "nz",  0 << 3 },
  { "z",   1 << 3 },
};

/* Parse condition code.  */
static const char *
parse_cc (const char *s, char *op)
{
  const char *p;
  int i;
  struct reg_entry * cc_p;

  for (i = 0; i < BUFLEN; ++i)
    {
      if (!ISALPHA (s[i])) /* Condition codes consist of letters only.  */
	break;
      buf[i] = TOLOWER (s[i]);
    }

  if ((i < BUFLEN)
      && ((s[i] == 0) || (s[i] == ',')))
    {
      buf[i] = 0;
      cc_p = bsearch (&key, cc_tab, ARRAY_SIZE (cc_tab),
		      sizeof (cc_tab[0]), key_cmp);
    }
  else
    cc_p = NULL;

  if (cc_p)
    {
      *op = cc_p->number;
      p = s + i;
    }
  else
    p = NULL;

  return p;
}

static const char *
emit_insn (char prefix, char opcode, const char *args)
{
  char *p;

  if (prefix)
    {
      p = frag_more (2);
      *p++ = prefix;
    }
  else
    p = frag_more (1);
  *p = opcode;
  return args;
}

void sm83_cons_fix_new (fragS *frag_p, int offset, int nbytes, expressionS *exp)
{
  bfd_reloc_code_real_type r[4] =
    {
      BFD_RELOC_8,
      BFD_RELOC_16,
      BFD_RELOC_24,
      BFD_RELOC_32
    };

  if (nbytes < 1 || nbytes > 4)
    {
      as_bad (_("unsupported BFD relocation size %u"), nbytes);
    }
  else
    {
      fix_new_exp (frag_p, offset, nbytes, exp, 0, r[nbytes-1]);
    }
}

static void
emit_data_val (expressionS *val, int size)
{
  char *p;
  bfd_reloc_code_real_type r_type;

  p = frag_more (size);
  if (val->X_op == O_constant)
    {
      int i;

       /* PR 28791:
	  Check for overflow, but ignore values that were generated by bit
	  manipulation operators (eg ~0xe6 and -7).  This does mean that
	  manipluated overlarge values will not be reported (eg ~0x1234),
	  but it does help to maintain compatibility with earlier versions
	  of the assembler.  */
      if (! val->X_extrabit
	  && is_overflow (val->X_add_number, size * 8))
	as_warn ( _("%d-bit overflow (%+" PRId64 ")"), size * 8,
		  (int64_t) val->X_add_number);
      for (i = 0; i < size; ++i)
	p[i] = (val->X_add_number >> (i * 8)) & 0xff;
      return;
    }

  switch (size)
    {
    case 1: r_type = BFD_RELOC_8; break;
    case 2: r_type = BFD_RELOC_16; break;
    case 3: r_type = BFD_RELOC_24; break;
    case 4: r_type = BFD_RELOC_32; break;
    case 8: r_type = BFD_RELOC_64; break;
    default:
      as_fatal (_("invalid data size %d"), size);
    }

  if (   (val->X_op == O_register)
      || (val->X_op == O_md1)
      || contains_register (val->X_add_symbol)
      || contains_register (val->X_op_symbol))
    {
      ill_op ();
      return;
    }

  if (size <= 2 && val->X_op_symbol)
    {
      bool simplify = true;
      int shift = symbol_get_value_expression (val->X_op_symbol)->X_add_number;
      if (val->X_op == O_bit_and && shift == (1 << (size*8))-1)
	shift = 0;
      else if (val->X_op != O_right_shift)
	shift = -1;

      if (size == 1)
	{
	  switch (shift)
	    {
	    case 0: r_type = BFD_RELOC_Z80_BYTE0; break;
	    case 8: r_type = BFD_RELOC_Z80_BYTE1; break;
	    case 16: r_type = BFD_RELOC_Z80_BYTE2; break;
	    case 24: r_type = BFD_RELOC_Z80_BYTE3; break;
	    default: simplify = false;
	    }
	}
      else /* if (size == 2) */
	{
	  switch (shift)
	    {
	    case 0: r_type = BFD_RELOC_Z80_WORD0; break;
	    case 16: r_type = BFD_RELOC_Z80_WORD1; break;
	    case 8:
	    case 24: /* add two byte fixups */
	      val->X_op = O_symbol;
	      val->X_op_symbol = NULL;
	      val->X_add_number = 0;
	      if (shift == 8)
		{
		  fix_new_exp (frag_now, p++ - frag_now->fr_literal, 1, val, false,
			       BFD_RELOC_Z80_BYTE1);
		  /* prepare to next byte */
		  r_type = BFD_RELOC_Z80_BYTE2;
		}
	      else
		r_type = BFD_RELOC_Z80_BYTE3; /* high byte will be 0 */
	      size = 1;
	      simplify = false;
	      break;
	    default: simplify = false;
	    }
	}

      if (simplify)
	{
	  val->X_op = O_symbol;
	  val->X_op_symbol = NULL;
	  val->X_add_number = 0;
	}
    }

  fix_new_exp (frag_now, p - frag_now->fr_literal, size, val, false, r_type);
}

static void
emit_byte (expressionS *val, bfd_reloc_code_real_type r_type)
{
  char *p;

  if (r_type == BFD_RELOC_8)
    {
      emit_data_val (val, 1);
      return;
    }
  p = frag_more (1);
  *p = val->X_add_number;
  if (contains_register (val->X_add_symbol) || contains_register (val->X_op_symbol))
    {
      ill_op ();
    }
  else if ((r_type == BFD_RELOC_8_PCREL) && (val->X_op == O_constant))
    {
      as_bad (_("cannot make a relative jump to an absolute location"));
    }
  else if (val->X_op == O_constant)
    {
      if ((val->X_add_number < -128) || (val->X_add_number >= 128))
	{
	  if (r_type == BFD_RELOC_Z80_DISP8)
	    as_bad (_("index overflow (%+" PRId64 ")"),
		    (int64_t) val->X_add_number);
	  else
	    as_bad (_("offset overflow (%+" PRId64 ")"),
		    (int64_t) val->X_add_number);
	}
    }
  else
    {
      /* For symbols only, constants are stored at begin of function.  */
      fix_new_exp (frag_now, p - frag_now->fr_literal, 1, val,
		   r_type == BFD_RELOC_8_PCREL, r_type);
    }
}

static void
emit_word (expressionS *val)
{
  emit_data_val (val, 2);
}

/* The operand m may be r, (hl).  */
static void
emit_mx (char prefix, char opcode, int shift, expressionS *arg)
{
  char *q;
  int rnum;

  rnum = arg->X_add_number;
  switch (arg->X_op)
    {
    case O_register:
      if (arg->X_md)
	{
	  if (rnum != REG_HL)
	    {
	      ill_op ();
	      break;
	    }
	  else
	    rnum = 6;
	}
      else
	{
	  if (rnum > 7)
	    {
	      ill_op ();
	      break;
	    }
	}
      q = frag_more (prefix ? 2 : 1);
      if (prefix)
	*q++ = prefix;
      *q++ = opcode + (rnum << shift);
      break;
    default:
      abort ();
    }
}

/* The operand m may be r, (hl).  */
static const char *
emit_m (char prefix, char opcode, const char *args)
{
  expressionS arg_m;
  const char *p;

  p = parse_exp (args, &arg_m);
  switch (arg_m.X_op)
    {
    case O_register:
      emit_mx (prefix, opcode, 0, &arg_m);
      break;
    default:
      ill_op ();
    }
  return p;
}

/* The operand s may be r, (hl), n.  */
static void
emit_sx (char opcode, expressionS *arg_p)
{
  char *q;

  switch (arg_p->X_op)
    {
    case O_register:
      emit_mx (0, opcode, 0, arg_p);
      break;
    default:
      if (arg_p->X_md || arg_p->X_op == O_add)
	ill_op ();
      else
	{
	  q = frag_more (1);
	  *q = opcode ^ 0x46;
	  emit_byte (arg_p, BFD_RELOC_8);
	}
    }
}

/* The operand s may be r, (hl), n.  */
static const char *
emit_s (char prefix ATTRIBUTE_UNUSED, char opcode, const char *args)
{
  expressionS arg_s;
  const char *p;

  p = parse_exp (args, &arg_s);
  if (*p == ',' && arg_s.X_md == 0 && arg_s.X_op == O_register && arg_s.X_add_number == REG_A)
    { /* possible instruction in generic format op A,x */
      ++p;
      p = parse_exp (p, &arg_s);
    }
  emit_sx (opcode, &arg_s);
  return p;
}

static const char *
emit_call (char opcode, const char *args)
{
  expressionS addr;
  const char *p;  char *q;

  p = parse_exp_not_indexed (args, &addr);
  if (addr.X_md)
    ill_op ();
  else
    {
      q = frag_more (1);
      *q = opcode;
      emit_word (&addr);
    }
  return p;
}

/* Operand may be rr, r, (hl).  */
static const char *
emit_incdec (char prefix, char opcode, const char * args)
{
  expressionS operand;
  int rnum;
  const char *p;  char *q;

  p = parse_exp (args, &operand);
  rnum = operand.X_add_number;
  if ((! operand.X_md)
      && (operand.X_op == O_register)
      && (R_ARITH & rnum))
    {
      q = frag_more (1);
      *q = prefix + ((rnum & 3) << 4);
    }
  else
    {
      if (operand.X_op == O_register)
	emit_mx (0, opcode, 3, &operand);
      else
	ill_op ();
    }
  return p;
}

static const char *
emit_jr (char opcode, const char *args)
{
  expressionS addr;
  const char *p;
  char *q;

  p = parse_exp_not_indexed (args, &addr);
  if (addr.X_md)
    ill_op ();
  else
    {
      q = frag_more (1);
      *q = opcode;
      addr.X_add_number--; /* pcrel computes after offset code */
      emit_byte (&addr, BFD_RELOC_8_PCREL);
    }
  return p;
}

static const char *
emit_jp (char prefix, char opcode, const char *args)
{
  expressionS addr;
  const char *p;
  char *q;

  p = parse_exp_not_indexed (args, & addr);
  if (O_register == addr.X_op)
    {
      if (REG_HL == addr.X_add_number)
	{
	  q = frag_more (1);
	  *q = prefix;
	}
      else
	ill_op ();
    }
  else
    {
      if (addr.X_md)
	ill_op ();
      else
	{
	  q = frag_more (1);
	  *q = opcode;
	  emit_word (&addr);
	}
    }
  return p;
}

static const char *
emit_pop (char prefix ATTRIBUTE_UNUSED, char opcode, const char *args)
{
  expressionS regp;
  const char *p;
  char *q;

  p = parse_exp (args, &regp);
  if ((!regp.X_md)
      && (regp.X_op == O_register)
      && (regp.X_add_number & R_STACKABLE))
    {
      int rnum;

      rnum = regp.X_add_number;
      q = frag_more (1);
      *q = opcode + ((rnum & 3) << 4);
    }
  else
    ill_op ();

  return p;
}

static const char *
emit_retcc (char prefix ATTRIBUTE_UNUSED, char opcode, const char *args)
{
  char cc, *q;
  const char *p;

  p = parse_cc (args, &cc);
  q = frag_more (1);
  if (p)
    *q = opcode + cc;
  else
    *q = prefix;
  return p ? p : args;
}

static const char *
emit_add (char prefix, char opcode, const char *args)
{
  expressionS term;
  int rhs;
  const char *p;
  char *q;

  p = parse_exp (args, &term);
  if (*p++ != ',')
    {
      error (_("bad instruction syntax"));
      return p;
    }

  if ((term.X_md) || (term.X_op != O_register))
    ill_op ();
  else
    switch (term.X_add_number)
      {
      case REG_A:
	p = parse_exp (p, &term);
	emit_sx (opcode, &term);
	break;
      case REG_SP:
	p = parse_exp (p, &term);
	if (term.X_md || term.X_op == O_register || term.X_op == O_add)
	  ill_op ();
	else
	  {
	    q = frag_more (1);
	    *q = 0xE8;
	    emit_byte (&term, BFD_RELOC_Z80_DISP8);
	  }
	break;
      case REG_HL:
	p = parse_exp (p, &term);
	rhs = term.X_add_number;
	if (term.X_md || term.X_op == O_md1)
	  ill_op ();
	else if ((term.X_op == O_register) && (rhs & R_ARITH))
	  {
	    q = frag_more (1);
	    *q = prefix + ((rhs & 3) << 4);
	    break;
	  }
	/* Fall through.  */
      default:
	ill_op ();
      }
  return p;
}

static const char *
emit_bit (char prefix, char opcode, const char *args)
{
  expressionS b;
  int bn;
  const char *p;

  p = parse_exp (args, &b);
  if (*p++ != ',')
    error (_("bad instruction syntax"));

  bn = b.X_add_number;
  if ((!b.X_md)
      && (b.X_op == O_constant)
      && (0 <= bn)
      && (bn < 8))
    {
      p = emit_m (prefix, opcode + (bn << 3), p);
    }
  else
    ill_op ();
  return p;
}

static const char *
emit_jpcc (char prefix, char opcode, const char *args)
{
  char cc;
  const char *p;

  p = parse_cc (args, &cc);
  if (p && *p++ == ',')
    p = emit_call (opcode + cc, p);
  else
    p = (prefix == (char)0xC3)
      ? emit_jp (0xE9, prefix, args)
      : emit_call (prefix, args);
  return p;
}

static const char *
emit_jrcc (char prefix, char opcode, const char *args)
{
  char cc;
  const char *p;

  p = parse_cc (args, &cc);
  if (p && *p++ == ',')
    p = emit_jr (opcode + cc, p);
  else
    p = emit_jr (prefix, args);

  return p;
}

static const char *
emit_rst (char prefix ATTRIBUTE_UNUSED, char opcode, const char *args)
{
  expressionS addr;
  const char *p;
  char *q;

  p = parse_exp_not_indexed (args, &addr);
  if (addr.X_op != O_constant)
    {
      error ("rst needs constant address");
      return p;
    }

  if (addr.X_add_number & ~(7 << 3))
    ill_op ();
  else
    {
      q = frag_more (1);
      *q = opcode + (addr.X_add_number & (7 << 3));
    }
  return p;
}

/* For 8-bit load register to memory instructions: LD (<expression>),r.  */
static void
emit_ld_m_r (expressionS *dst, expressionS *src)
{
  char *q;

  switch (dst->X_op)
    {
    case O_md1:
      break;
    case O_register:
      switch (dst->X_add_number)
	{
	case REG_BC: /* LD (BC),A */
	case REG_DE: /* LD (DE),A */
	  if (src->X_add_number == REG_A)
	    {
	      q = frag_more (1);
	      *q = 0x02 | ((dst->X_add_number & 3) << 4);
	      return;
	    }
	  break;
	case REG_HL: /* LD (HL),r */
	  if (src->X_add_number <= 7)
	    {
	      q = frag_more (1);
	      *q = 0x70 | src->X_add_number;
	      return;
	    }
	  break;
	case REG_HLI: /* LD (HLI),A */
	case REG_HLD: /* LD (HLD),A */
	  if (src->X_add_number == REG_A)
	    {
	      q = frag_more (1);
	      *q = dst->X_add_number;
	      return;
	    }
	  break;
	case REG_C: /* LD (C),A */
	  if (src->X_add_number == REG_A)
	    {
	      q = frag_more (1);
	      *q = 0xE2;
	      return;
	    }
	  break;
	default:;
	}
	break;
    default: /* LD (n),A (or LD (nn),A if SDCC compat) */
      if (src->X_add_number == REG_A)
	{
	  q = frag_more (1);
	  if (sdcc_compat)
	    {
	      *q = 0xEA; /* LD (nn),A (aka. LDX) */
	      emit_word (dst);
	    }
	  else
	    {
	      *q = 0xE0; /* LD (n),A (aka. LDH) */
	      emit_byte (dst, BFD_RELOC_8);
	    }
	  return;
	}
      break;
    }
    ill_op ();
}

/* For 16-bit load register to memory instructions: LD (<expression>),rr.  */
static void
emit_ld_m_rr (expressionS *dst, expressionS *src)
{
  char *q;

  switch (dst->X_op)
    {
    case O_register:
    case O_md1:
      ill_op ();
      break;
    default: /* LD (nn),rr */
      if (src->X_add_number == REG_SP) /* LD (nn),SP */
	{
	  q = frag_more (1);
	  *q = 0x08;
	  emit_word (dst);
	}
      else
	ill_op ();
    }
}

/* For 8-bit memory load to register: LD r,(xxx). */
static void
emit_ld_r_m (expressionS *dst, expressionS *src)
{
  char *q;
  char opcode = 0;

  if (dst->X_add_number == REG_A && src->X_op == O_register)
    {
      switch (src->X_add_number)
	{
	case REG_BC:  opcode = 0x0A; break; /* LD A,(BC) */
	case REG_DE:  opcode = 0x1A; break; /* LD A,(DE) */
	case REG_HLI:                                           /* LD A,(HLI) */
	case REG_HLD: opcode = 0x08 | src->X_add_number; break; /* LD A,(HLD) */
	case REG_C:   opcode = 0xF2; break; /* LD A,(C) */
	default: break;
	}
      if (opcode != 0)
	{
	  q = frag_more (1);
	  *q = opcode;
	  return;
	}
    }

  switch (src->X_op)
    {
    case O_md1:
      ill_op ();
      break;
    case O_register:
      if (src->X_add_number == REG_HL) /* LD r,(HL) */
	{
	  opcode = 0x46; /* LD B,(HL) */
	  q = frag_more (1);
	  *q = opcode | ((dst->X_add_number & 7) << 3);
	}
      else
	ill_op ();
      break;
    default: /* LD A,(n) (or LD A,(nn) if SDCC compat) */
      if (dst->X_add_number == REG_A)
	{
	  q = frag_more (1);
	  if (sdcc_compat)
	    {
	      *q = 0xFA; /* LD A,(nn) (aka. LDX) */
	      emit_word (src);
	    }
	  else
	    {
	      *q = 0xF0; /* LD A,(n) (aka. LDH) */
	      emit_byte (src, BFD_RELOC_8);
	    }
	}
      else
	ill_op ();
    }
}

/* For 8-bit immediate value load to register: LD r,n. */
static void
emit_ld_r_n (expressionS *dst, expressionS *src)
{
  char *q;

  q = frag_more (1);
  *q = 0x06 | ((dst->X_add_number & 7) << 3);
  emit_byte (src, BFD_RELOC_8);
}

/* Mostly 8-bit load register from register instructions: LD r,r. */
/* There is one exception: LD SP,HL */
static void
emit_ld_r_r (expressionS *dst, expressionS *src)
{
  char *q;
  int opcode = 0;

  if (dst->X_add_number == REG_SP && src->X_add_number == REG_HL)
    opcode = 0xF9;
  else if (dst->X_add_number <= 7 && src->X_add_number <= 7)
    opcode = 0x40 + ((dst->X_add_number & 7) << 3) + (src->X_add_number & 7);
  else
    {
      ill_op ();
      return;
    }

  q = frag_more (1);
  *q = opcode;
}

/* For 16-bit immediate value load to register: LD rr,nn or LD HL,SP+d. */
static void
emit_ld_rr_nn (expressionS *dst, expressionS *src)
{
  char *q;
  int opcode;

  if (dst->X_add_number == REG_HL
      && src->X_op == O_add
      && src->X_add_number == REG_SP
      && src->X_add_symbol) /* LD HL,SP+d */
    {
      expressionS offset = *src;
      offset.X_op = O_symbol;
      offset.X_add_number = 0;

      q = frag_more (1);
      *q = 0xF8;

      emit_byte (&offset, BFD_RELOC_Z80_DISP8);
      return;
    }
  else if (src->X_op == O_add)
    {
      ill_op ();
      return;
    }

  switch (dst->X_add_number)
    {
    case REG_BC:
    case REG_DE:
    case REG_HL:
    case REG_SP:
      opcode = 0x01 + ((dst->X_add_number & 3) << 4);
      break;
    default:
      ill_op ();
      return;
    }
  q = frag_more (1);
  *q = opcode;
  emit_word (src);
}

static const char *
emit_ld (char prefix_in ATTRIBUTE_UNUSED, char opcode_in ATTRIBUTE_UNUSED,
	 const char * args)
{
  expressionS dst, src;
  const char *p;
  char *q;

  p = parse_exp (args, &dst);
  if (*p++ != ',')
    error (_("bad instruction syntax"));
  p = parse_exp (p, &src);

  if (dst.X_md)
    {
      if (src.X_op == O_register)
	{
	  if (src.X_add_number <= 7)
	    emit_ld_m_r (&dst, &src); /* LD (xxx),r */
	  else
	    emit_ld_m_rr (&dst, &src); /* LD (xxx),rr */
	}
      else if (dst.X_op == O_register && dst.X_add_number == REG_HL)
	{
	  q = frag_more (1);
	  *q = 0x36; /* LD (hl),n */
	  emit_byte (&src, BFD_RELOC_8);
	}
      else
	ill_op ();
    }
  else if (dst.X_op == O_register)
    {
      if (src.X_md)
	{
	  if (dst.X_add_number <= 7)
	    emit_ld_r_m (&dst, &src);
	  else
	    ill_op ();
	}
      else if (src.X_op == O_register)
	emit_ld_r_r (&dst, &src);
      else if (src.X_op == O_add)
	ill_op ();
      else if (dst.X_add_number <= 7)
	emit_ld_r_n (&dst, &src);
      else
	emit_ld_rr_nn (&dst, &src);
    }
  else
    ill_op ();

  return p;
}

static const char *
emit_lddldi (char prefix ATTRIBUTE_UNUSED, char opcode, const char *args)
{
  expressionS dst, src;
  const char *p;
  char *q;

  p = parse_exp (args, &dst);
  if (*p++ != ',')
    error (_("bad instruction syntax"));
  p = parse_exp (p, &src);

  if (dst.X_op != O_register || src.X_op != O_register)
    {
      ill_op ();
      return p;
    }

  if (dst.X_md != 0
      && dst.X_add_number == REG_HL
      && src.X_md == 0
      && src.X_add_number == REG_A)
    opcode |= 0x00; /* LDx (HL),A */
  else if (dst.X_md == 0
      && dst.X_add_number == REG_A
      && src.X_md != 0
      && src.X_add_number == REG_HL)
    opcode |= 0x08; /* LDx A,(HL) */
  else
    {
      ill_op ();
      return p;
    }

  q = frag_more (1);
  *q = opcode;
  return p;
}

static const char *
emit_ldh (char prefix ATTRIBUTE_UNUSED, char opcode ATTRIBUTE_UNUSED,
	  const char *args)
{
  expressionS dst, src;
  const char *p;
  char *q;

  p = parse_exp (args, &dst);
  if (*p++ != ',')
    {
      error (_("bad instruction syntax"));
      return p;
    }

  p = parse_exp (p, &src);
  if (dst.X_md == 0
      && dst.X_op == O_register
      && dst.X_add_number == REG_A
      && src.X_md != 0
      && src.X_op != O_md1)
    {
      if (src.X_op != O_register) /* LDH A,(n) */
	{
	  q = frag_more (1);
	  *q = 0xF0;
	  emit_byte (&src, BFD_RELOC_8);
	}
      else if (src.X_add_number == REG_C) /* LDH A,(C) */
	*frag_more (1) = 0xF2;
      else
	ill_op ();
    }
  else if (dst.X_md != 0
      && dst.X_op != O_md1
      && src.X_md == 0
      && src.X_op == O_register
      && src.X_add_number == REG_A)
    {
      if (dst.X_op != O_register) /* LDH (n),A */
	{
	  q = frag_more (1);
	  *q = 0xE0;
	  emit_byte (&dst, BFD_RELOC_8);
	}
      else if (dst.X_add_number == REG_C) /* LDH (C),A */
	*frag_more (1) = 0xE2;
      else
	ill_op ();
    }
  else
    ill_op ();

  return p;
}

static const char *
emit_ldhl (char prefix ATTRIBUTE_UNUSED, char opcode, const char *args)
{
  expressionS dst, src;
  const char *p;
  char *q;
  p = parse_exp (args, &dst);
  if (*p++ != ',')
    {
      error (_("bad instruction syntax"));
      return p;
    }

  p = parse_exp (p, &src);
  if (dst.X_md || dst.X_op != O_register || dst.X_add_number != REG_SP
      || src.X_md || src.X_op == O_register || src.X_op == O_md1 || src.X_op == O_add)
    ill_op ();
  else
    {
      q = frag_more (1);
      *q = opcode;
      emit_byte (&src, BFD_RELOC_Z80_DISP8);
    }
  return p;
}

static void
emit_data (int size ATTRIBUTE_UNUSED)
{
  const char *p, *q;
  char *u, quote;
  int cnt;
  expressionS exp;

  if (is_it_end_of_statement ())
    {
      demand_empty_rest_of_line ();
      return;
    }
  p = skip_space (input_line_pointer);

  do
    {
      if (*p == '\"' || *p == '\'')
	{
	    for (quote = *p, q = ++p, cnt = 0; *p && quote != *p; ++p, ++cnt)
	      ;
	    u = frag_more (cnt);
	    memcpy (u, q, cnt);
	    if (!*p)
	      as_warn (_("unterminated string"));
	    else
	      p = skip_space (p+1);
	}
      else
	{
	  p = parse_exp (p, &exp);
	  if (exp.X_op == O_md1 || exp.X_op == O_register || exp.X_op == O_add)
	    {
	      ill_op ();
	      break;
	    }
	  if (exp.X_md)
	    as_warn (_("parentheses ignored"));
	  emit_byte (&exp, BFD_RELOC_8);
	  p = skip_space (p);
	}
    }
  while (*p++ == ',') ;
  input_line_pointer = (char *)(p-1);
}

static const char *
emit_ldx (char prefix ATTRIBUTE_UNUSED, char opcode ATTRIBUTE_UNUSED,
	  const char *args)
{
  expressionS dst, src;
  const char *p;
  char *q;

  p = parse_exp (args, &dst);
  if (*p++ != ',')
    {
      error (_("bad instruction syntax"));
      return p;
    }

  p = parse_exp (p, &src);
  if (dst.X_md == 0
      && dst.X_op == O_register
      && dst.X_add_number == REG_A
      && src.X_md != 0
      && src.X_op != O_register
      && src.X_op != O_md1)
    {
      q = frag_more (1);
      *q = 0xFA; /* LDX A,(nn) */
      emit_word (&src);
    }
  else if (dst.X_md != 0
      && dst.X_op != O_register
      && dst.X_op != O_md1
      && src.X_md == 0
      && src.X_op == O_register
      && src.X_add_number == REG_A)
    {
      q = frag_more (1);
      *q = 0xEA; /* LDX (nn),A */
      emit_word (&dst);
    }
  else
    ill_op ();

  return p;
}

static void
sm83_cons (int size)
{
  const char *p;
  expressionS exp;

  if (is_it_end_of_statement ())
    {
      demand_empty_rest_of_line ();
      return;
    }
  p = skip_space (input_line_pointer);

  do
    {
      p = parse_exp (p, &exp);
      if (exp.X_op == O_md1 || exp.X_op == O_register || exp.X_op == O_add)
	{
	  ill_op ();
	  break;
	}
      if (exp.X_md)
	as_warn (_("parentheses ignored"));
      emit_data_val (&exp, size);
      p = skip_space (p);
  } while (*p++ == ',') ;
  input_line_pointer = (char *)(p-1);
}

static void
psect (int arg)
{
#if defined(OBJ_ELF)
  return obj_elf_section (arg);
#elif defined(OBJ_COFF)
  return obj_coff_section (arg);
#else
#error Unknown object format
#endif
}

static void
set_inss (int inss)
{
  sdcc_compat = !!inss;
}

static void
ignore (int arg ATTRIBUTE_UNUSED)
{
  ignore_rest_of_line ();
}

static void
area (int arg)
{
  char *p;
  if (!sdcc_compat)
    as_fatal (_("Invalid directive"));
  for (p = input_line_pointer; *p && *p != '(' && *p != '\n'; p++)
    ;
  if (*p == '(')
    {
      *p = '\n';
      psect (arg);
      *p++ = '(';
      ignore_rest_of_line ();
    }
  else
    psect (arg);
}

/* Port specific pseudo ops.  */
const pseudo_typeS md_pseudo_table[] =
{
  { ".area",	area,		0 },
  { ".gbz80",	set_inss,	1 },
  { ".module",	ignore,		0 },
  { ".optsdcc",	ignore,		0 },
  { ".set",	s_set,		0 },
  { ".sm83",	set_inss,	0 },
  { "db" ,	emit_data,	1 },
  { "d24",	sm83_cons,	3 },
  { "d32",	sm83_cons,	4 },
  { "def24",	sm83_cons,	3 },
  { "def32",	sm83_cons,	4 },
  { "defb",	emit_data,	1 },
  { "defm",	emit_data,	1 },
  { "defs",	s_space,	1 }, /* Synonym for ds on some assemblers.  */
  { "defw",	sm83_cons,	2 },
  { "ds",	s_space,	1 }, /* Fill with bytes rather than words.  */
  { "dw",	sm83_cons,	2 },
  { "psect",	psect,		0 }, /* TODO: Translate attributes.  */
  { "set",	NULL,		0 }, /* Real instruction on z80.  */
  { "xdef",	s_globl,	0 }, /* Synonym for .GLOBAL */
  { "xref",	s_ignore,	0 }, /* Synonym for .EXTERN */

  { NULL, NULL, 0 }
} ;

static table_t instab[] =
{
  { "adc",  0x00, 0x88, emit_s      },
  { "add",  0x09, 0x80, emit_add    },
  { "and",  0x00, 0xA0, emit_s      },
  { "bit",  0xCB, 0x40, emit_bit    },
  { "call", 0xCD, 0xC4, emit_jpcc   },
  { "ccf",  0x00, 0x3F, emit_insn   },
  { "cp",   0x00, 0xB8, emit_s      },
  { "cpl",  0x00, 0x2F, emit_insn   },
  { "daa",  0x00, 0x27, emit_insn   },
  { "dec",  0x0B, 0x05, emit_incdec },
  { "di",   0x00, 0xF3, emit_insn   },
  { "ei",   0x00, 0xFB, emit_insn   },
  { "halt", 0x00, 0x76, emit_insn   },
  { "inc",  0x03, 0x04, emit_incdec },
  { "jp",   0xC3, 0xC2, emit_jpcc   },
  { "jr",   0x18, 0x20, emit_jrcc   },
  { "ld",   0x00, 0x00, emit_ld     },
  { "ldd",  0x00, 0x32, emit_lddldi },
  { "ldh",  0x00, 0x00, emit_ldh    },
  { "ldhl", 0x00, 0xF8, emit_ldhl   },
  { "ldi",  0x00, 0x22, emit_lddldi },
  { "ldx",  0x00, 0x00, emit_ldx    },
  { "nop",  0x00, 0x00, emit_insn   },
  { "or",   0x00, 0xB0, emit_s      },
  { "pop",  0x00, 0xC1, emit_pop    },
  { "push", 0x00, 0xC5, emit_pop    },
  { "res",  0xCB, 0x80, emit_bit    },
  { "ret",  0xC9, 0xC0, emit_retcc  },
  { "reti", 0x00, 0xD9, emit_insn   },
  { "rl",   0xCB, 0x10, emit_m      },
  { "rla",  0x00, 0x17, emit_insn   },
  { "rlc",  0xCB, 0x00, emit_m      },
  { "rlca", 0x00, 0x07, emit_insn   },
  { "rr",   0xCB, 0x18, emit_m      },
  { "rra",  0x00, 0x1F, emit_insn   },
  { "rrc",  0xCB, 0x08, emit_m      },
  { "rrca", 0x00, 0x0F, emit_insn   },
  { "rst",  0x00, 0xC7, emit_rst    },
  { "sbc",  0x00, 0x98, emit_s      },
  { "scf",  0x00, 0x37, emit_insn   },
  { "set",  0xCB, 0xC0, emit_bit    },
  { "sla",  0xCB, 0x20, emit_m      },
  { "sra",  0xCB, 0x28, emit_m      },
  { "srl",  0xCB, 0x38, emit_m      },
  { "stop", 0x00, 0x10, emit_insn   },
  { "sub",  0x00, 0x90, emit_s      },
  { "swap", 0xCB, 0x30, emit_m      },
  { "xor",  0x00, 0xA8, emit_s      },
};

void
md_assemble (char *str)
{
  const char *p;
  char *old_ptr;
  int i;
  table_t *insp;

  err_flag = 0;
  old_ptr = input_line_pointer;
  p = skip_space (str);
  for (i = 0; (i < BUFLEN) && (ISALPHA (*p) || ISDIGIT (*p));)
    buf[i++] = TOLOWER (*p++);

  if (i == BUFLEN)
    {
      buf[BUFLEN-3] = buf[BUFLEN-2] = '.'; /* Mark opcode as abbreviated.  */
      buf[BUFLEN-1] = 0;
      as_bad (_("Unknown instruction '%s'"), buf);
    }
  else
    {
      dwarf2_emit_insn (0);
      if ((*p) && !is_whitespace (*p))
	{
	  as_bad (_("syntax error"));
	  goto end;
	}
      buf[i] = 0;
      p = skip_space (p);
      key = buf;

      insp = bsearch (&key, instab, ARRAY_SIZE (instab),
		    sizeof (instab[0]), key_cmp);
      if (!insp)
	{
	  *frag_more (1) = 0;
	  as_bad (_("Unknown instruction `%s'"), buf);
	}
      else
	{
	  p = insp->fp (insp->prefix, insp->opcode, p);
	  p = skip_space (p);
	  if ((!err_flag) && *p)
	    as_bad (_("junk at end of line, "
		      "first unrecognized character is `%c'"), *p);
	}
    }
 end:
  input_line_pointer = old_ptr;
}

static int
signed_overflow (signed long value, unsigned bitsize)
{
  signed long max = (signed long) ((1UL << (bitsize - 1)) - 1);
  return value < -max - 1 || value > max;
}

static int
unsigned_overflow (unsigned long value, unsigned bitsize)
{
  return value >> (bitsize - 1) >> 1 != 0;
}

static int
is_overflow (long value, unsigned bitsize)
{
  if (value < 0)
    return signed_overflow (value, bitsize);
  return unsigned_overflow (value, bitsize);
}

void
md_apply_fix (fixS *fixP, valueT *valP, segT seg)
{
  long val = *valP;
  char *p_lit = fixP->fx_where + fixP->fx_frag->fr_literal;

  if (fixP->fx_addsy == NULL)
    fixP->fx_done = 1;
  else if (fixP->fx_pcrel)
    {
      segT s = S_GET_SEGMENT (fixP->fx_addsy);
      if (s == seg || s == absolute_section)
	{
	  val += S_GET_VALUE (fixP->fx_addsy);
	  fixP->fx_done = 1;
	}
    }

  switch (fixP->fx_r_type)
    {
    case BFD_RELOC_8_PCREL:
    case BFD_RELOC_Z80_DISP8:
    case BFD_RELOC_8:
    case BFD_RELOC_16:
    case BFD_RELOC_24:
    case BFD_RELOC_32:
      fixP->fx_no_overflow = 0;
      break;
    default:
      fixP->fx_no_overflow = 1;
      break;
    }

  switch (fixP->fx_r_type)
    {
    case BFD_RELOC_8_PCREL:
    case BFD_RELOC_Z80_DISP8:
      if (fixP->fx_done && signed_overflow (val, 8))
	as_bad_where (fixP->fx_file, fixP->fx_line,
		      _("8-bit signed offset out of range (%+ld)"), val);
      *p_lit++ = val;
      break;

    case BFD_RELOC_Z80_BYTE0:
      *p_lit++ = val;
      break;

    case BFD_RELOC_Z80_BYTE1:
      *p_lit++ = (val >> 8);
      break;

    case BFD_RELOC_Z80_BYTE2:
      *p_lit++ = (val >> 16);
      break;

    case BFD_RELOC_Z80_BYTE3:
      *p_lit++ = (val >> 24);
      break;

    case BFD_RELOC_8:
      if (fixP->fx_done && is_overflow(val, 8))
	as_warn_where (fixP->fx_file, fixP->fx_line,
		       _("8-bit overflow (%+ld)"), val);
      *p_lit++ = val;
      break;

    case BFD_RELOC_Z80_WORD1:
      *p_lit++ = (val >> 16);
      *p_lit++ = (val >> 24);
      break;

    case BFD_RELOC_Z80_WORD0:
      *p_lit++ = val;
      *p_lit++ = (val >> 8);
      break;

    case BFD_RELOC_16:
      if (fixP->fx_done && is_overflow(val, 16))
	as_warn_where (fixP->fx_file, fixP->fx_line,
		       _("16-bit overflow (%+ld)"), val);
      *p_lit++ = val;
      *p_lit++ = (val >> 8);
      break;

    case BFD_RELOC_24: /* Def24 may produce this.  */
      if (fixP->fx_done && is_overflow(val, 24))
	as_warn_where (fixP->fx_file, fixP->fx_line,
		       _("24-bit overflow (%+ld)"), val);
      *p_lit++ = val;
      *p_lit++ = (val >> 8);
      *p_lit++ = (val >> 16);
      break;

    case BFD_RELOC_32: /* Def32 and .long may produce this.  */
      if (fixP->fx_done && is_overflow(val, 32))
	as_warn_where (fixP->fx_file, fixP->fx_line,
		       _("32-bit overflow (%+ld)"), val);
      *p_lit++ = val;
      *p_lit++ = (val >> 8);
      *p_lit++ = (val >> 16);
      *p_lit++ = (val >> 24);
      break;

    default:
      printf (_("md_apply_fix: unknown reloc type 0x%x\n"), fixP->fx_r_type);
      abort ();
    }
}

/* GAS will call this to generate a reloc.  GAS will pass the
   resulting reloc to `bfd_install_relocation'.  This currently works
   poorly, as `bfd_install_relocation' often does the wrong thing, and
   instances of `tc_gen_reloc' have been written to work around the
   problems, which in turns makes it difficult to fix
   `bfd_install_relocation'.  */

/* If while processing a fixup, a reloc really
   needs to be created then it is done here.  */

arelent *
tc_gen_reloc (asection *seg ATTRIBUTE_UNUSED , fixS *fixp)
{
  arelent *reloc;

  if (fixp->fx_subsy != NULL)
    {
      as_bad_subtract (fixp);
      return NULL;
    }

  reloc = notes_alloc (sizeof (arelent));
  reloc->sym_ptr_ptr = notes_alloc (sizeof (asymbol *));
  *reloc->sym_ptr_ptr = symbol_get_bfdsym (fixp->fx_addsy);
  reloc->address = fixp->fx_frag->fr_address + fixp->fx_where;
  reloc->addend = fixp->fx_offset;
  reloc->howto = bfd_reloc_type_lookup (stdoutput, fixp->fx_r_type);
  if (reloc->howto == NULL)
    {
      as_bad_where (fixp->fx_file, fixp->fx_line,
		    _("reloc %d not supported by object file format"),
		    (int) fixp->fx_r_type);
      return NULL;
    }

  if (fixp->fx_r_type == BFD_RELOC_VTABLE_INHERIT
      || fixp->fx_r_type == BFD_RELOC_VTABLE_ENTRY)
    reloc->address = fixp->fx_offset;

  return reloc;
}

int
sm83_tc_labels_without_colon (void)
{
  return colonless_labels;
}

int
sm83_tc_label_is_local (const char *name)
{
  const char *n;
  const char *p;
  if (local_label_prefix == NULL)
    return 0;
  for (p = local_label_prefix, n = name; *p && *n && *n == *p; p++, n++)
    ;
  return *p == '\0';
}

/* Parse floating point number from string and compute mantissa and
   exponent. Mantissa is normalized.
*/
#define EXP_MIN -0x10000
#define EXP_MAX 0x10000
static int
str_to_broken_float (bool *signP, uint64_t *mantissaP, int *expP)
{
  char *p;
  bool sign;
  uint64_t mantissa = 0;
  int exponent = 0;
  int i;

  p = (char*)skip_space (input_line_pointer);
  sign = (*p == '-');
  *signP = sign;
  if (sign || *p == '+')
    ++p;
  if (strncasecmp (p, "NaN", 3) == 0)
    {
      *mantissaP = 0;
      *expP = 0;
      input_line_pointer = p + 3;
      return 1;
    }
  if (strncasecmp (p, "inf", 3) == 0)
    {
      *mantissaP = 1ull << 63;
      *expP = EXP_MAX;
      input_line_pointer = p + 3;
      return 1;
    }
  for (; ISDIGIT (*p); ++p)
    {
      if (mantissa >> 60)
	{
	  if (*p >= '5')
	    mantissa++;
	  break;
	}
      mantissa = mantissa * 10 + (*p - '0');
    }
  /* skip non-significant digits */
  for (; ISDIGIT (*p); ++p)
    exponent++;

  if (*p == '.')
    {
      p++;
      if (!exponent) /* If no precision overflow.  */
	{
	  for (; ISDIGIT (*p); ++p, --exponent)
	    {
	      if (mantissa >> 60)
		{
		  if (*p >= '5')
		    mantissa++;
		  break;
		}
	      mantissa = mantissa * 10 + (*p - '0');
	    }
	}
      for (; ISDIGIT (*p); ++p)
	;
    }
  if (*p == 'e' || *p == 'E')
    {
      int es;
      int t = 0;
      ++p;
      es = (*p == '-');
      if (es || *p == '+')
        p++;
      for (; ISDIGIT (*p); ++p)
	{
	  if (t < 100)
	    t = t * 10 + (*p - '0');
	}
      exponent += (es) ? -t : t;
    }
  if (ISALNUM (*p) || *p == '.')
    return 0;
  input_line_pointer = p;
  if (mantissa == 0)
    {
      *mantissaP = 1ull << 63;
      *expP = EXP_MIN;
      return 1; /* result is 0 */
    }
  /* normalization */
  for (; mantissa <= ~0ull/10; --exponent)
    mantissa *= 10;
  /* Now we have sign, mantissa, and signed decimal exponent
     need to recompute to binary exponent.  */
  for (i = 64; exponent > 0; --exponent)
    {
      /* be sure that no integer overflow */
      while (mantissa > ~0ull/10)
	{
	  mantissa >>= 1;
	  i += 1;
	}
	mantissa *= 10;
    }
  for (; exponent < 0; ++exponent)
    {
      while (!(mantissa >> 63))
	{
	  mantissa <<= 1;
	  i -= 1;
	}
	mantissa /= 10;
    }
  /* normalization */
  for (; !(mantissa >> 63); --i)
    mantissa <<= 1;
  *mantissaP = mantissa;
  *expP = i;
  return 1;
}

static const char *
str_to_zeda32(char *litP, int *sizeP)
{
  uint64_t mantissa;
  bool sign;
  int exponent;
  unsigned i;

  *sizeP = 4;
  if (!str_to_broken_float (&sign, &mantissa, &exponent))
    return _("invalid syntax");
  /* I do not know why decrement is needed */
  --exponent;
  /* shift by 39 bits right keeping 25 bit mantissa for rounding */
  mantissa >>= 39;
  /* do rounding */
  ++mantissa;
  /* make 24 bit mantissa */
  mantissa >>= 1;
  /* check for overflow */
  if (mantissa >> 24)
    {
      mantissa >>= 1;
      ++exponent;
    }
  /* check for 0 */
  if (exponent < -127)
    {
      exponent = -128;
      mantissa = 0;
    }
  else if (exponent > 127)
    {
      exponent = -128;
      mantissa = sign ? 0xc00000 : 0x400000;
    }
  else if (mantissa == 0)
    {
      exponent = -128;
      mantissa = 0x200000;
    }
  else if (!sign)
    mantissa &= (1ull << 23) - 1;
  for (i = 0; i < 24; i += 8)
    *litP++ = mantissa >> i;
  *litP = 0x80 + exponent;
  return NULL;
}

/*
  Math48 by Anders Hejlsberg support.
  Mantissa is 39 bits wide, exponent 8 bit wide.
  Format is:
  bit 47: sign
  bit 46-8: normalized mantissa (bits 38-0, bit39 assumed to be 1)
  bit 7-0: exponent+128 (0 - value is null)
  MIN: 2.938735877e-39
  MAX: 1.701411835e+38
*/
static const char *
str_to_float48(char *litP, int *sizeP)
{
  uint64_t mantissa;
  bool sign;
  int exponent;
  unsigned i;

  *sizeP = 6;
  if (!str_to_broken_float (&sign, &mantissa, &exponent))
    return _("invalid syntax");
  /* shift by 23 bits right keeping 41 bit mantissa for rounding */
  mantissa >>= 23;
  /* do rounding */
  ++mantissa;
  /* make 40 bit mantissa */
  mantissa >>= 1;
  /* check for overflow */
  if (mantissa >> 40)
    {
      mantissa >>= 1;
      ++exponent;
    }
  if (exponent < -127)
    {
      memset (litP, 0, 6);
      return NULL;
    }
  if (exponent > 127)
    return _("overflow");
  if (!sign)
    mantissa &= (1ull << 39) - 1;
  *litP++ = 0x80 + exponent;
  for (i = 0; i < 40; i += 8)
    *litP++ = mantissa >> i;
  return NULL;
}

static const char *
str_to_ieee754_h(char *litP, int *sizeP)
{
  return ieee_md_atof ('h', litP, sizeP, false);
}

static const char *
str_to_ieee754_s(char *litP, int *sizeP)
{
  return ieee_md_atof ('s', litP, sizeP, false);
}

static const char *
str_to_ieee754_d(char *litP, int *sizeP)
{
  return ieee_md_atof ('d', litP, sizeP, false);
}

#ifdef TARGET_USE_CFIPOP
/* Initialize the DWARF-2 unwind information for this procedure. */
void
sm83_tc_frame_initial_instructions (void)
{
  static int sp_regno = -1;

  if (sp_regno < 0)
    sp_regno = sm83_tc_regname_to_dw2regnum ("sp");

  cfi_add_CFA_def_cfa (sp_regno, 0);
}

int
sm83_tc_regname_to_dw2regnum (const char *regname)
{
  static const char *regs[] =
    { /* same registers as for GDB */
      "af", "bc", "de", "hl",
      "sp", "pc"
    };
  unsigned i;

  for (i = 0; i < ARRAY_SIZE(regs); ++i)
    if (!strcasecmp (regs[i], regname))
      return i;

  return -1;
}
#endif
