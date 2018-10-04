/* tc-lr35902.c -- Assemble code for the Sharp LR35902 (GameBoy CPU)
   Copyright (C) 2005-2018 Free Software Foundation, Inc.

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

/* Exported constants. */
const char comment_chars[] = ";";
const char line_comment_chars[] = "#;";
const char line_separator_chars[] = "";
const char EXP_CHARS[] = "eE";
const char FLT_CHARS[] = "RrFf";

/* For machine specific options. */
const char *md_shortopts = ""; /* None yet. */

struct option md_longopts[] =
{
  { NULL, no_argument, NULL, 0 }
};

size_t md_longopts_size = sizeof md_longopts;

int md_parse_option(int c ATTRIBUTE_UNUSED, const char *arg ATTRIBUTE_UNUSED)
{
  return 1;
}

void md_show_usage(FILE *f ATTRIBUTE_UNUSED)
{
}

static symbolS *zero;

struct reg_entry
{
  const char *name;
  int        number;
};

#define R_STACKABLE (0x80)
#define R_ARITH     (0x40)

#define REG_A (7)
#define REG_B (0)
#define REG_C (1)
#define REG_D (2)
#define REG_E (3)
#define REG_H (4)
#define REG_L (5)
#define REG_F (6 | 8)

#define REG_AF (3 | R_STACKABLE)
#define REG_BC (0 | R_STACKABLE | R_ARITH)
#define REG_DE (1 | R_STACKABLE | R_ARITH)
#define REG_HL (2 | R_STACKABLE | R_ARITH)
#define REG_SP (3 | R_ARITH)

static const struct reg_entry regtable[] =
{
  {"a",  REG_A },
  {"af", REG_AF },
  {"b",  REG_B },
  {"bc", REG_BC },
  {"c",  REG_C },
  {"d",  REG_D },
  {"de", REG_DE },
  {"e",  REG_E },
  {"f",  REG_F },
  {"h",  REG_H },
  {"hl", REG_HL },
  {"l",  REG_L },
  {"sp", REG_SP },
};

#define BUFLEN 8 /* Large enough for any keyword. */

void md_begin(void)
{
  expressionS  nul, reg;
  char         *p;
  unsigned int i, j, k;
  char         buf[BUFLEN];

  reg.X_op         = O_register;
  reg.X_md         = 0;
  reg.X_add_symbol = 0;
  reg.X_op_symbol  = 0;

  for (i = 0; i < ARRAY_SIZE(regtable); ++i)
  {
    reg.X_add_number = regtable[i].number;
    k = strlen(regtable[i].name);
    buf[k] = 0;
    if (k + 1 < BUFLEN)
    {
      for (j = 1 << k; j; --j)
      {
        for (k = 0; regtable[i].name[k]; ++k)
        {
          buf[k] = (j & (1 << k)) ? TOUPPER(regtable[i].name[k]) : regtable[i].name[k];
        }
        symbolS *psym = symbol_find_or_make(buf);
        S_SET_SEGMENT(psym, reg_section);
        symbol_set_value_expression(psym, &reg);
      }
    }
  }

  p = input_line_pointer;
  input_line_pointer = (char *)"0";
  nul.X_md = 0;
  expression(&nul);
  input_line_pointer = p;
  zero = make_expr_symbol(&nul);

  /* We do not use relaxation (yet). */
  linkrelax = 0;
}

static const char *skip_space(const char *s)
{
  while (*s == ' ' || *s == '\t')
    ++s;
  return s;
}

/* A non-zero return-value causes a continue in the
   function read_a_source_file() in ../read.c. */
int lr35902_start_line_hook(void)
{
  char *p, quote;
  char buf[4];

  /* Convert one character constants. */
  for (p = input_line_pointer; *p && *p != '\n'; ++p)
  {
    switch (*p)
    {
    case '\'':
      if (p[1] && p[1] != '\'' && p[2] == '\'')
      {
        snprintf(buf, 4, "%3d", (int)(unsigned char)p[1]);
        *p++ = buf[0];
        *p++ = buf[1];
        *p++ = buf[2];
        break;
      }
      /* Fall through. */
    case '"':
      for (quote = *p++; quote != *p && '\n' != *p; ++p)
        /* No escapes. */ ;
      if (quote != *p)
      {
        as_bad(_("-- unterminated string"));
        ignore_rest_of_line();
        return 1;
      }
      break;
    }
  }
  /* Check for <label>[:] [.](EQU|DEFL) <value>. */
  if (is_name_beginner(*input_line_pointer))
  {
    char *name, *rest, *line_start;
    int  len;
    char c;

    line_start = input_line_pointer;
    if (ignore_input())
      return 0;

    c = get_symbol_name(&name);
    rest = input_line_pointer + 1;

    if (*rest == ':')
      ++rest;
    if (*rest == ' ' || *rest == '\t')
      ++rest;
    if (*rest == '.')
      ++rest;
    if (strncasecmp(rest, "EQU", 3) == 0)
      len = 3;
    else if (strncasecmp(rest, "DEFL", 4) == 0)
      len = 4;
    else
      len = 0;
    if (len && (!ISALPHA(rest[len])))
    {
      /* Handle assignment here. */
      if (line_start[-1] == '\n')
      {
        bump_line_counters();
        LISTING_NEWLINE();
      }
      input_line_pointer = rest + len - 1;
      /* Allow redefining with "DEFL" (len == 4), but not with "EQU". */
      equals(name, len == 4);
      return 1;
    }
    else
    {
      /* Restore line and pointer. */
      (void)restore_line_pointer(c);
      input_line_pointer = line_start;
    }
  }
  return 0;
}

symbolS *md_undefined_symbol(char *name ATTRIBUTE_UNUSED)
{
  return NULL;
}

const char *md_atof(int  type   ATTRIBUTE_UNUSED,
                    char *litP  ATTRIBUTE_UNUSED,
                    int  *sizeP ATTRIBUTE_UNUSED)
{
  return _("floating point numbers are not implemented");
}

valueT md_section_align(segT seg ATTRIBUTE_UNUSED, valueT size)
{
  return size;
}

long md_pcrel_from(fixS *fixp)
{
  return fixp->fx_where + fixp->fx_frag->fr_address + 1;
}

typedef const char *(asfunc)(char, char, const char *);

typedef struct _table_t
{
  const char    *name;
  unsigned char prefix;
  unsigned char opcode;
  asfunc        *fp;
} table_t;

/* Compares the key for structs that start with a char * to the key. */
static int key_cmp(const void *a, const void *b)
{
  const char *str_a, *str_b;

  str_a = *((const char **)a);
  str_b = *((const char **)b);
  return strcmp(str_a, str_b);
}

char buf[BUFLEN];
const char *key = buf;

/* Prevent an error on a line from also generating
   a "junk at end of line" error message. */
static char err_flag;

static void error(const char *message)
{
  as_bad("%s", message);
  err_flag = 1;
}

static void ill_op(void)
{
  error(_("illegal operand"));
}

/* Check whether an expression is indirect. */
static int is_indir(const char *s)
{
  const char *p;
  int        indir, depth;
  char       quote;

  /* Indirection is indicated with parentheses. */
  indir = *s == '(';

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
      ++depth;
      break;
    case ')':
      --depth;
      if (depth == 0)
      {
        p = skip_space(p + 1);
        if (*p && *p != ',')
          indir = 0;
        --p;
      }
      if (depth < 0)
        error(_("mismatched parentheses"));
      break;
    }
  }

  if (depth != 0)
    error(_("mismatched parentheses"));

  return indir;
}

/* Check whether a symbol involves a register. */
static int contains_register(symbolS *sym)
{
  if (sym)
  {
    expressionS *ex = symbol_get_value_expression(sym);
    return O_register == ex->X_op ||
           (ex->X_add_symbol && contains_register(ex->X_add_symbol)) ||
           (ex->X_op_symbol && contains_register(ex->X_op_symbol));
  }
  else
    return 0;
}

/* Parse general expression. */
static const char *parse_exp(const char *s, expressionS *op)
{
  const char *p;

  p = skip_space(s);
  op->X_md = is_indir(p);
  input_line_pointer = (char *)s;
  expression(op);
  switch (op->X_op)
  {
  case O_absent:
    error(_("missing operand"));
    break;
  case O_illegal:
    error(_("bad expression syntax"));
    break;
  default:
    break;
  }
  return input_line_pointer;
}

/* Condition codes, including some synonyms provided by HiTech zas. */
static const struct reg_entry cc_tab[] =
{
  { "c",   3 << 3 },
  { "lge", 2 << 3 },
  { "llt", 3 << 3 },
  { "nc",  2 << 3 },
  { "nz",  0 << 3 },
  { "z",   1 << 3 }
} ;

/* Parse condition code. */
static const char *parse_cc(const char *s, char *op)
{
  const char       *p;
  int              i;
  struct reg_entry *cc_p;

  for (i = 0; i < BUFLEN; ++i)
  {
    if (!ISALPHA(s[i])) /* Condition codes consist of letters only. */
      break;
    buf[i] = TOLOWER(s[i]);
  }

  if ((i < BUFLEN) && ((s[i] == 0) || (s[i] == ',')))
  {
    buf[i] = 0;
    cc_p = bsearch(&key, cc_tab, ARRAY_SIZE(cc_tab),
                   sizeof cc_tab[0], key_cmp);
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

static const char *emit_insn(char prefix, char opcode, const char *args)
{
  char *p;

  if (prefix)
  {
    p = frag_more(2);
    *p++ = prefix;
  }
  else
    p = frag_more(1);
  *p = opcode;
  return args;
}

void lr35902_cons_fix_new(fragS *frag_p, int offset, int nbytes, expressionS *exp)
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
    as_bad(_("unsupported BFD relocation size %u"), nbytes);
  }
  else
  {
    fix_new_exp(frag_p, offset, nbytes, exp, 0, r[nbytes - 1]);
  }
}

static void emit_byte(expressionS *val, bfd_reloc_code_real_type r_type)
{
  char *p;
  int lo, hi;

  p = frag_more(1);
  *p = val->X_add_number;
  if (contains_register(val->X_add_symbol) || contains_register(val->X_op_symbol))
  {
    ill_op();
  }
  else if ((r_type == BFD_RELOC_8_PCREL) && (val->X_op == O_constant))
  {
    as_bad(_("cannot make a relative jump to an absolute location"));
  }
  else if (val->X_op == O_constant)
  {
    lo = -128;
    hi = (BFD_RELOC_8 == r_type) ? 255 : 127;

    if ((val->X_add_number < lo) || (val->X_add_number > hi))
    {
      if (r_type == BFD_RELOC_Z80_DISP8)
        as_bad(_("offset too large"));
      else
        as_warn(_("overflow"));
    }
  }
  else
  {
    fix_new_exp(frag_now, p - frag_now->fr_literal, 1, val,
                (r_type == BFD_RELOC_8_PCREL) ? TRUE : FALSE, r_type);
    /* FIXME : Process constant offsets immediately. */
  }
}

static void emit_word(expressionS *val)
{
  char *p;

  p = frag_more(2);
  if (val->X_op == O_register ||
      contains_register(val->X_add_symbol) ||
      contains_register(val->X_op_symbol))
    ill_op();
  else
  {
    *p = val->X_add_number;
    p[1] = val->X_add_number >> 8;
    if (val->X_op != O_constant)
      fix_new_exp(frag_now, p - frag_now->fr_literal, 2,
                  val, FALSE, BFD_RELOC_16);
  }
}

/* The operand m may be r, (hl). */
static void emit_mx(char prefix, char opcode, int shift, expressionS *arg)
{
  char *q;
  int  rnum;

  rnum = arg->X_add_number;
  switch (arg->X_op)
  {
  case O_register:
    if (arg->X_md)
    {
      if (rnum != REG_HL)
      {
        ill_op();
        break;
      }
      else
        rnum = 6;
    }
    else
    {
      if (rnum > 7)
      {
        ill_op();
        break;
      }
    }
    q = frag_more(prefix ? 2 : 1);
    if (prefix)
      *q++ = prefix;
    *q++ = opcode + (rnum << shift);
    break;
  default:
    abort();
  }
}

/* The operand m may be r, (hl). */
static const char *emit_m(char prefix, char opcode, const char *args)
{
  expressionS arg_m;
  const char  *p;

  p = parse_exp(args, &arg_m);
  switch (arg_m.X_op)
  {
  case O_register:
    emit_mx(prefix, opcode, 0, &arg_m);
    break;
  default:
    ill_op();
  }
  return p;
}

/* The operand s may be r, (hl), n. */
static void emit_sx(char prefix, char opcode, expressionS *arg_p)
{
  char *q;

  switch (arg_p->X_op)
  {
  case O_register:
    emit_mx(prefix, opcode, 0, arg_p);
    break;
  default:
    if (arg_p->X_md)
      ill_op();
    else
    {
      q = frag_more(prefix ? 2 : 1);
      if (prefix)
        *q++ = prefix;
      *q = opcode ^ 0x46;
      emit_byte(arg_p, BFD_RELOC_8);
    }
  }
}

/* The operand s may be r, (hl), n. */
static const char *emit_s(char prefix, char opcode, const char *args)
{
  expressionS arg_s;
  const char  *p;

  p = parse_exp(args, &arg_s);
  emit_sx(prefix, opcode, &arg_s);
  return p;
}

static const char *emit_call(char opcode, const char *args)
{
  expressionS addr;
  const char  *p;
  char        *q;

  p = parse_exp(args, &addr);
  if (addr.X_md)
    ill_op();
  else
  {
    q = frag_more(1);
    *q = opcode;
    emit_word(&addr);
  }
  return p;
}

/* Operand may be rr, r, (hl). */
static const char *emit_incdec(char prefix, char opcode, const char *args)
{
  expressionS operand;
  const char  *p;
  char        *q;
  int         rnum;

  p = parse_exp(args, &operand);
  rnum = operand.X_add_number;
  if (!operand.X_md &&
      operand.X_op == O_register &&
      R_ARITH & rnum)
  {
    q = frag_more(1);
    *q = prefix + ((rnum & 3) << 4);
  }
  else
  {
    if (operand.X_op == O_register)
      emit_mx(0, opcode, 3, &operand);
    else
      ill_op();
  }
  return p;
}

static const char *emit_jr(char opcode, const char *args)
{
  expressionS addr;
  const char  *p;
  char        *q;

  p = parse_exp(args, &addr);
  if (addr.X_md)
    ill_op();
  else
  {
    q = frag_more(1);
    *q = opcode;
    emit_byte(&addr, BFD_RELOC_8_PCREL);
  }
  return p;
}

static const char *emit_jp(char prefix, char opcode, const char *args)
{
  expressionS addr;
  const char  *p;
  char        *q;
  int         rnum;

  p = parse_exp(args, &addr);
  if (addr.X_md)
  {
    rnum = addr.X_add_number;
    if ((O_register == addr.X_op) && (REG_HL == rnum))
    {
      q = frag_more(1);
      *q = prefix;
    }
    else
      ill_op();
  }
  else
  {
    q = frag_more(1);
    *q = opcode;
    emit_word(&addr);
  }
  return p;
}

static const char *emit_pop(char prefix ATTRIBUTE_UNUSED, char opcode, const char *args)
{
  expressionS regp;
  const char  *p;
  char        *q;

  p = parse_exp(args, &regp);
  if (!regp.X_md &&
      regp.X_op == O_register &&
      regp.X_add_number & R_STACKABLE)
  {
    int rnum;

    rnum = regp.X_add_number;
    q = frag_more(1);
    *q = opcode + ((rnum & 3) << 4);
  }
  else
    ill_op();

  return p;
}

static const char *emit_retcc(char prefix, char opcode, const char *args)
{
  const char *p;
  char       *q, cc;

  p = parse_cc(args, &cc);
  q = frag_more(1);
  if (p)
    *q = opcode + cc;
  else
    *q = prefix;
  return p ? p : args;
}

static const char *emit_adc(char prefix ATTRIBUTE_UNUSED, char opcode, const char *args)
{
  expressionS term;
  const char  *p;

  p = parse_exp(args, &term);
  if (*p++ != ',')
  {
    error(_("bad instruction syntax"));
    return p;
  }

  if (term.X_md || term.X_op != O_register)
    ill_op();
  else
    switch (term.X_add_number)
    {
    case REG_A:
      p = emit_s(0, opcode, p);
      break;
    default:
      ill_op();
    }
  return p;
}

static const char *emit_add(char prefix, char opcode, const char *args)
{
  expressionS term;
  const char  *p;
  char        *q;
  int         rhs;

  p = parse_exp(args, &term);
  if (*p++ != ',')
  {
    error(_("bad instruction syntax"));
    return p;
  }

  if (term.X_md || term.X_op != O_register)
    ill_op();
  else
    switch (term.X_add_number)
    {
    case REG_A:
      p = emit_s(0, opcode, p);
      break;
    case REG_SP:
      p = parse_exp(p, &term);
      if (!term.X_md && term.X_op != O_register)
      {
        q = frag_more(1);
        *q = 0xE8;
        emit_byte(&term, BFD_RELOC_Z80_DISP8);
        break;
      }
      ill_op();
      break;
    case REG_HL:
      p = parse_exp(p, &term);
      if (!term.X_md && term.X_op == O_register)
      {
        rhs = term.X_add_number;
        if (rhs & R_ARITH)
        {
          q = frag_more(1);
          *q = prefix + ((rhs & 3) << 4);
          break;
        }
      }
      /* Fall through. */
    default:
      ill_op();
    }
  return p;
}

static const char *emit_bit(char prefix, char opcode, const char *args)
{
  expressionS b;
  const char  *p;
  int         bn;

  p = parse_exp(args, &b);
  if (*p++ != ',')
    error(_("bad instruction syntax"));

  bn = b.X_add_number;
  if (!b.X_md &&
      b.X_op == O_constant &&
      bn >= 0 && bn < 8)
  {
    p = emit_m(prefix, opcode + (bn << 3), p);
  }
  else
    ill_op();
  return p;
}

static const char *emit_jpcc(char prefix, char opcode, const char *args)
{
  const char *p;
  char       cc;

  p = parse_cc(args, &cc);
  if (p && *p++ == ',')
    p = emit_call(opcode + cc, p);
  else
    p = (prefix == (char)0xC3)
      ? emit_jp(0xE9, prefix, args)
      : emit_call(prefix, args);
  return p;
}

static const char *emit_jrcc(char prefix, char opcode, const char *args)
{
  const char *p;
  char       cc;

  p = parse_cc(args, &cc);
  if (p && *p++ == ',')
    p = emit_jr(opcode + cc, p);
  else
    p = emit_jr(prefix, args);

  return p;
}

static const char *emit_rst(char prefix ATTRIBUTE_UNUSED, char opcode, const char *args)
{
  expressionS addr;
  const char  *p;
  char        *q;

  p = parse_exp(args, &addr);
  if (addr.X_op != O_constant)
  {
    error("rst needs constant address");
    return p;
  }

  if (addr.X_add_number & ~(7 << 3))
    ill_op();
  else
  {
    q = frag_more(1);
    *q = opcode + (addr.X_add_number & (7 << 3));
  }
  return p;
}

/* Called for "ld (hl), *". */
static void emit_ldxhl(expressionS *src)
{
  char *q;

  if (src->X_md)
  {
    ill_op();
    return;
  }

  if (src->X_op == O_register)
  {
    if (src->X_add_number > 7)
    {
      ill_op();
      return;
    }
    q = frag_more(1);
    *q = 0x70 + src->X_add_number;
  }
  else
  {
    q = frag_more(1);
    *q = 0x36;
    emit_byte(src, BFD_RELOC_8);
  }
}

static void parse_sp_index(expressionS *op)
{
  switch (op->X_op)
  {
  case O_add:
  case O_subtract:
    if (!op->X_md && O_register == symbol_get_value_expression(op->X_add_symbol)->X_op)
    {
      int rnum = symbol_get_value_expression(op->X_add_symbol)->X_add_number;
      if (REG_SP != rnum || contains_register(op->X_op_symbol))
      {
        ill_op();
        break;
      }
      if (O_subtract == op->X_op)
      {
        expressionS minus;
        minus.X_op = O_uminus;
        minus.X_add_number = 0;
        minus.X_add_symbol = op->X_op_symbol;
        minus.X_op_symbol = 0;
        op->X_op_symbol = make_expr_symbol(&minus);
        op->X_op = O_add;
      }
      symbol_get_value_expression(op->X_op_symbol)->X_add_number += op->X_add_number;
      op->X_add_number = 0;
      op->X_add_symbol = op->X_op_symbol;
      op->X_op_symbol = 0;
      op->X_op = O_md1;
    }
    break;
  case O_register:
    if (!op->X_md && REG_SP == op->X_add_number)
    {
      op->X_add_number = 0;
      op->X_add_symbol = zero;
      op->X_op = O_md1;
    }
    break;
  default:
    break;
  }
}

/* Called for "ld r, *". */
static void emit_ldreg(int dest, expressionS *src)
{
  char *q;

  switch (dest)
  {
  /* 8 Bit ld group: */
  case REG_A:
    if (src->X_md && src->X_op != O_register)
    {
      q = frag_more(1);
      *q = 0xFA;
      emit_word(src);
      break;
    }

    if (src->X_md && src->X_op == O_register &&
        (src->X_add_number == REG_BC || src->X_add_number == REG_DE))
    {
      q = frag_more(1);
      *q = 0x0A + ((src->X_add_number & 1) << 4);
      break;
    }

    /* Fall through. */
  case REG_B:
  case REG_C:
  case REG_D:
  case REG_E:
  case REG_H:
  case REG_L:
    emit_sx (0, 0x40 + (dest << 3), src);
    break;

  /* 16 Bit ld group: */
  case REG_SP:
    if (!src->X_md &&
        src->X_op == O_register &&
        REG_HL == src->X_add_number)
    {
      q = frag_more(1);
      *q = 0xF9;
      break;
    }
    /* Fall through. */
  case REG_BC:
  case REG_DE:
    if (src->X_md || src->X_op == O_register)
    {
      ill_op();
      break;
    }
    q = frag_more(1);
    *q = 0x01 + ((dest & 3) << 4);
    emit_word(src);
    break;

  case REG_HL:
    parse_sp_index(src);
    if (src->X_md || src->X_op == O_register)
    {
      ill_op();
      break;
    }
    if (src->X_op == O_md1)
    {
      expressionS src_offset = *src;
      src_offset.X_op = O_symbol;
      q = frag_more(1);
      *q = 0xF8;
      emit_byte(&src_offset, BFD_RELOC_Z80_DISP8);
    }
    else
    {
      q = frag_more(1);
      *q = 0x21;
      emit_word(src);
    }
    break;

  case REG_AF:
  case REG_F:
    ill_op();
    break;

  default:
    abort();
  }
}

static const char *emit_ld(char prefix_in ATTRIBUTE_UNUSED,
                           char opcode_in ATTRIBUTE_UNUSED,
                           const char *args)
{
  expressionS dst, src;
  const char  *p;
  char        *q;
  char        opcode;

  p = parse_exp(args, &dst);
  if (*p++ != ',')
    error (_("bad instruction syntax"));
  p = parse_exp(p, &src);

  switch (dst.X_op)
  {
  case O_register:
    if (dst.X_md)
    {
      switch (dst.X_add_number)
      {
      case REG_BC:
      case REG_DE:
        if (!src.X_md && src.X_op == O_register && src.X_add_number == REG_A)
        {
          q = frag_more(1);
          *q = 0x02 + ((dst.X_add_number & 1) << 4);
        }
        else
          ill_op();
        break;
      case REG_HL:
        emit_ldxhl(&src);
        break;
      default:
        ill_op();
      }
    }
    else
      emit_ldreg(dst.X_add_number, &src);
    break;

  default:
    if (src.X_md || src.X_op != O_register)
      ill_op();
    opcode = 0;
    switch (src.X_add_number)
    {
    case REG_A:
      opcode = 0xEA; break;
    case REG_SP:
      opcode = 0x08; break;
    }
    if (opcode)
    {
      q = frag_more(1);
      *q = opcode;
      emit_word(&dst);
    }
    else
      ill_op();
  }
  return p;
}

static const char *emit_ldh(char prefix ATTRIBUTE_UNUSED,
                            char opcode ATTRIBUTE_UNUSED,
                            const char *args)
{
  expressionS dst, src;
  const char  *p;
  char        *q;

  p = parse_exp(args, &dst);
  if (*p++ != ',')
    error (_("bad instruction syntax"));
  p = parse_exp(p, &src);

  switch (dst.X_op)
  {
  case O_register:
    switch (dst.X_add_number)
    {
    case REG_C:
      if (dst.X_md && !src.X_md &&
          src.X_op == O_register && src.X_add_number == REG_A)
      {
        q = frag_more(1);
        *q = 0xE2;
      }
      else
        ill_op();
      break;
    case REG_A:
      if (!dst.X_md && src.X_md)
      {
        if (src.X_op == O_register)
        {
          if (src.X_add_number == REG_C)
          {
            q = frag_more(1);
            *q = 0xF2;
            break;
          }
        }
        else
        {
          q = frag_more(1);
          *q = 0xF0;
          emit_byte(&src, BFD_RELOC_8);
          break;
        }
      }
      ill_op();
      break;
    default:
      ill_op();
    }
    break;

  default:
    if (dst.X_md && !src.X_md &&
        src.X_op == O_register && src.X_add_number == REG_A)
    {
      q = frag_more(1);
      *q = 0xE0;
      emit_byte(&dst, BFD_RELOC_8);
    }
    else
      ill_op();
    break;
  }
  return p;
}

static const char *emit_ldid(char prefix,
                             char opcode,
                             const char *args)
{
  expressionS dst, src;
  const char  *p;
  char        *q;

  p = parse_exp(args, &dst);
  if (*p++ != ',')
    error (_("bad instruction syntax"));
  p = parse_exp(p, &src);

  switch (dst.X_op)
  {
  case O_register:
    switch (dst.X_add_number)
    {
    case REG_HL:
      if (dst.X_md && !src.X_md &&
          src.X_op == O_register && src.X_add_number == REG_A)
      {
        q = frag_more(1);
        *q = prefix;
      }
      else
        ill_op();
      break;
    case REG_A:
      if (!dst.X_md && src.X_md &&
          src.X_op == O_register && src.X_add_number == REG_HL)
      {
        q = frag_more(1);
        *q = opcode;
      }
      else
        ill_op();
      break;
    default:
      ill_op();
    }
    break;

  default:
    ill_op();
  }
  return p;
}

static void emit_data(int size ATTRIBUTE_UNUSED)
{
  expressionS exp;
  const char  *p, *q;
  char        *u;
  int         cnt;
  char        quote;

  if (is_it_end_of_statement())
  {
    demand_empty_rest_of_line();
    return;
  }
  p = skip_space(input_line_pointer);

  do
  {
    if (*p == '\"' || *p == '\'')
    {
      for (quote = *p, q = ++p, cnt = 0; *p && quote != *p; ++p, ++cnt);
      u = frag_more(cnt);
      memcpy(u, q, cnt);
      if (!*p)
        as_warn(_("unterminated string"));
      else
        p = skip_space(p + 1);
    }
    else
    {
      p = parse_exp(p, &exp);
      if (exp.X_op == O_register)
      {
        ill_op();
        break;
      }
      if (exp.X_md)
        as_warn(_("parentheses ignored"));
      emit_byte(&exp, BFD_RELOC_8);
      p = skip_space(p);
    }
  } while (*p++ == ',');
  input_line_pointer = (char *)(p - 1);
}

/* Port specific pseudo ops. */
const pseudo_typeS md_pseudo_table[] =
{
  { "db" ,   emit_data,        1 },
  { "d24",   cons,             3 },
  { "d32",   cons,             4 },
  { "def24", cons,             3 },
  { "def32", cons,             4 },
  { "defb",  emit_data,        1 },
  { "defs",  s_space,          1 }, /* Synonym for ds on some assemblers. */
  { "defw",  cons,             2 },
  { "ds",    s_space,          1 }, /* Fill with bytes rather than words. */
  { "dw",    cons,             2 },
  { "psect", obj_coff_section, 0 }, /* TODO: Translate attributes. */
  { "set",   NULL,             0 }, /* Real instruction on Z80 (and LR35902). */
  { NULL,    NULL,             0 }
} ;

static table_t instab[] =
{
  { "adc",  0x00, 0x88, emit_adc },
  { "add",  0x09, 0x80, emit_add },
  { "and",  0x00, 0xA0, emit_s },
  { "bit",  0xCB, 0x40, emit_bit },
  { "call", 0xCD, 0xC4, emit_jpcc },
  { "ccf",  0x00, 0x3F, emit_insn },
  { "cp",   0x00, 0xB8, emit_s },
  { "cpl",  0x00, 0x2F, emit_insn },
  { "daa",  0x00, 0x27, emit_insn },
  { "dec",  0x0B, 0x05, emit_incdec },
  { "di",   0x00, 0xF3, emit_insn },
  { "ei",   0x00, 0xFB, emit_insn },
  { "halt", 0x00, 0x76, emit_insn },
  { "inc",  0x03, 0x04, emit_incdec },
  { "jp",   0xC3, 0xC2, emit_jpcc },
  { "jr",   0x18, 0x20, emit_jrcc },
  { "ld",   0x00, 0x00, emit_ld },
  { "ldd",  0x32, 0x3A, emit_ldid },
  { "ldh",  0x00, 0x00, emit_ldh },
  { "ldi",  0x22, 0x2A, emit_ldid },
  { "nop",  0x00, 0x00, emit_insn },
  { "or",   0x00, 0xB0, emit_s },
  { "pop",  0x00, 0xC1, emit_pop },
  { "push", 0x00, 0xC5, emit_pop },
  { "res",  0xCB, 0x80, emit_bit },
  { "ret",  0xC9, 0xC0, emit_retcc },
  { "reti", 0x00, 0xD9, emit_insn },
  { "rl",   0xCB, 0x10, emit_m },
  { "rla",  0x00, 0x17, emit_insn },
  { "rlc",  0xCB, 0x00, emit_m },
  { "rlca", 0x00, 0x07, emit_insn },
  { "rr",   0xCB, 0x18, emit_m },
  { "rra",  0x00, 0x1F, emit_insn },
  { "rrc",  0xCB, 0x08, emit_m },
  { "rrca", 0x00, 0x0F, emit_insn },
  { "rst",  0x00, 0xC7, emit_rst},
  { "sbc",  0x00, 0x98, emit_adc },
  { "scf",  0x00, 0x37, emit_insn },
  { "set",  0xCB, 0xC0, emit_bit },
  { "sla",  0xCB, 0x20, emit_m },
  { "sra",  0xCB, 0x28, emit_m },
  { "srl",  0xCB, 0x38, emit_m },
  { "stop", 0x00, 0x10, emit_insn },
  { "sub",  0x00, 0x90, emit_s },
  { "swap", 0xCB, 0x30, emit_m },
  { "xor",  0x00, 0xA8, emit_s }
} ;

void md_assemble(char *str)
{
  const char *p;
  char       *old_ptr;
  table_t    *insp;
  int        i;

  err_flag = 0;
  old_ptr = input_line_pointer;
  p = skip_space(str);
  for (i = 0; i < BUFLEN && ISALPHA(*p);)
    buf[i++] = TOLOWER(*p++);

  if (i == BUFLEN)
  {
    buf[BUFLEN - 3] = buf[BUFLEN - 2] = '.'; /* Mark opcode as abbreviated. */
    buf[BUFLEN - 1] = 0;
    as_bad(_("Unknown instruction '%s'"), buf);
  }
  else if (*p && !ISSPACE(*p))
    as_bad(_("syntax error"));
  else
  {
    buf[i] = 0;
    p = skip_space(p);
    key = buf;

    insp = bsearch(&key, instab, ARRAY_SIZE(instab),
                   sizeof instab[0], key_cmp);
    if (!insp)
      as_bad(_("Unknown instruction '%s'"), buf);
    else
    {
      p = insp->fp(insp->prefix, insp->opcode, p);
      p = skip_space(p);
      if (!err_flag && *p)
        as_bad(_("junk at end of line, first unrecognized character is `%c'"), *p);
    }
  }
  input_line_pointer = old_ptr;
}

void md_apply_fix(fixS *fixP, valueT *valP, segT seg ATTRIBUTE_UNUSED)
{
  char *p_lit = fixP->fx_where + fixP->fx_frag->fr_literal;
  long val = *(long *)valP;

  switch (fixP->fx_r_type)
  {
  case BFD_RELOC_8_PCREL:
    if (fixP->fx_addsy)
    {
      fixP->fx_no_overflow = 1;
      fixP->fx_done = 0;
    }
    else
    {
      fixP->fx_no_overflow = (-128 <= val && val < 128);
      if (!fixP->fx_no_overflow)
        as_bad_where(fixP->fx_file, fixP->fx_line,
                     _("relative jump out of range"));
      *p_lit++ = val;
      fixP->fx_done = 1;
    }
    break;

  case BFD_RELOC_Z80_DISP8:
    if (fixP->fx_addsy)
    {
      fixP->fx_no_overflow = 1;
      fixP->fx_done = 0;
    }
    else
    {
      fixP->fx_no_overflow = (-128 <= val && val < 128);
      if (!fixP->fx_no_overflow)
        as_bad_where(fixP->fx_file, fixP->fx_line,
                     _("index offset out of range"));
      *p_lit++ = val;
      fixP->fx_done = 1;
    }
    break;

  case BFD_RELOC_8:
    if (val > 255 || val < -128)
      as_warn_where(fixP->fx_file, fixP->fx_line, _("overflow"));
    *p_lit++ = val;
    fixP->fx_no_overflow = 1;
    if (fixP->fx_addsy == NULL)
      fixP->fx_done = 1;
    break;

  case BFD_RELOC_16:
    *p_lit++ = val;
    *p_lit++ = (val >> 8);
    fixP->fx_no_overflow = 1;
    if (fixP->fx_addsy == NULL)
      fixP->fx_done = 1;
    break;

  case BFD_RELOC_24: /* Def24 may produce this. */
    *p_lit++ = val;
    *p_lit++ = (val >> 8);
    *p_lit++ = (val >> 16);
    fixP->fx_no_overflow = 1;
    if (fixP->fx_addsy == NULL)
      fixP->fx_done = 1;
    break;

  case BFD_RELOC_32: /* Def32 and .long may produce this. */
    *p_lit++ = val;
    *p_lit++ = (val >> 8);
    *p_lit++ = (val >> 16);
    *p_lit++ = (val >> 24);
    if (fixP->fx_addsy == NULL)
      fixP->fx_done = 1;
    break;

  default:
    printf(_("md_apply_fix: unknown r_type 0x%x\n"), fixP->fx_r_type);
    abort();
  }
}

/* GAS will call this to generate a reloc.  GAS will pass the
   resulting reloc to `bfd_install_relocation'.  This currently works
   poorly, as `bfd_install_relocation' often does the wrong thing, and
   instances of `tc_gen_reloc' have been written to work around the
   problems, which in turns makes it difficult to fix
   `bfd_install_relocation'. */

/* If while processing a fixup, a reloc really
   needs to be created then it is done here. */

arelent *tc_gen_reloc(asection *seg ATTRIBUTE_UNUSED, fixS *fixp)
{
  arelent *reloc;

  if (!bfd_reloc_type_lookup(stdoutput, fixp->fx_r_type))
  {
    as_bad_where(fixp->fx_file, fixp->fx_line,
                 _("reloc %d not supported by object file format"),
                 (int)fixp->fx_r_type);
    return NULL;
  }

  reloc               = XNEW(arelent);
  reloc->sym_ptr_ptr  = XNEW(asymbol *);
  *reloc->sym_ptr_ptr = symbol_get_bfdsym(fixp->fx_addsy);
  reloc->address      = fixp->fx_frag->fr_address + fixp->fx_where;
  reloc->howto        = bfd_reloc_type_lookup(stdoutput, fixp->fx_r_type);
  reloc->addend       = fixp->fx_offset;

  return reloc;
}
