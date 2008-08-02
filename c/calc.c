#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include "asm.h"


static u8 buf[256];
static x86_bufptr x86_bptr = buf + sizeof buf;
static i32 x86_tmp;             /* ugh */

typedef int (*func)();

i32 stack[256];

static void
dump_code (void)
{
  x86_bufptr bp = x86_bptr;
  for (; bp < buf + sizeof buf; ++bp)
    printf ("%02x ", *bp);
  printf ("\n");
}


static void
x86_epilog (void)
{
  x86_pop_gv (ebp);
  x86_pop_gv (ebx);
  x86_pop_gv (esi);
  x86_pop_gv (edi);
}

static void
x86_prolog (void)
{
  x86_mov_gv_iv (esi, (i32) stack);

  x86_push_gv (edi);
  x86_push_gv (esi);
  x86_push_gv (ebx);
  x86_mov_gv_ev (ebp, reg (esp));
  x86_push_gv (ebp);
}


enum {
  HALT, PUSH, ADD, SUB, MUL, DIV, MOD
};

int depth = 1;

static void
pop_gv (int r)
{
  x86_mov_gv_ev (r, at_reg (esi));
  x86_sub_ev_iv (reg (esi), 4);
}

static void
push_gv (int r)
{
  x86_add_ev_iv (reg (esi), 4);
  x86_mov_ev_gv (at_reg (esi), r);
}


static void
gen_push (i32 i)
{
  if (0) printf ("%d push %d\n", depth, i);
  x86_mov_gv_iv (eax, i);
  if (0 < depth)
    push_gv (eax);
}

static void
gen (u8 opcode)
{
  switch (opcode)
    {
    case HALT: 
      if (0) printf ("%d halt\n", depth);
      x86_ret ();
      x86_epilog ();
      break;

    case ADD: 
      if (0) printf ("%d +\n", depth);
      x86_add_gv_ev (eax, reg (ebx));
      pop_gv (ebx);
      break;

    case SUB:
      if (0) printf ("%d -\n", depth);
      x86_sub_gv_ev (eax, reg (ebx));
      pop_gv (eax);
      x86_mov_gv_ev (ebx, reg (eax));
      break;

    case MUL:
      if (0) printf ("%d *\n", depth);
      x86_imul_ev (reg (ebx));
      pop_gv (ebx);
      break;

    case DIV:
      if (0) printf ("%d /\n", depth);
      x86_idiv_ev (reg (ebx));
      x86_mov_gv_iv (edx, 0);
      pop_gv (eax);
      x86_mov_gv_ev (ebx, reg (eax));
      break;

    case MOD:
      if (0) printf ("%d %%\n", depth);
      x86_mov_gv_ev (eax, reg (edx));
      x86_idiv_ev (reg (ebx));
      x86_mov_gv_iv (edx, 0);
      pop_gv (eax);
      x86_mov_gv_ev (ebx, reg (eax));
      break;

    default:
      assert (0);
    }
}


static i32 
run (void)
{
  func test_func = (func) x86_bptr;
  return test_func ();
}


struct token {
  int tag;
  int value;
};

static const char *start, *scan;
static struct token tok;

static int
cook_number (const char *p, const char *q)
{
  char buf[20];
  memcpy (buf, p, q-p);
  buf[q-p] = '\0';
  return strtol (buf, NULL, 10);
}

static int
scan_number (void)
{
  const char *end = scan;
  while (start < scan && isdigit (scan[-1]))
    --scan;
  return cook_number (scan, end);
}

static void
next (void) 
{
  while (start < scan && isspace (scan[-1]))
    --scan;

  if (scan == start)
    tok.tag = '\0';
  else
    switch (scan[-1])
      {
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        tok.tag = PUSH;
        tok.value = scan_number ();
        break;

      case '+': 
      case '-': 
      case '*': 
      case '/': 
      case '%': 
      case '(':
      case ')':
        tok.tag = *--scan;
        break;

      default:
        printf ("Whoops\n");
        exit (0);
      }
}

static void
emit (struct token t)
{
  switch (t.tag)
    {
    case PUSH:
      --depth;
      gen_push (t.value);
      break;

    case '+': ++depth; gen (ADD); break;
    case '-': ++depth; gen (SUB); break;
    case '*': ++depth; gen (MUL); break;
    case '/': ++depth; gen (DIV); break;
    case '%': ++depth; gen (MOD); break;

    default:
      printf ("Whoops\n");
      exit (0);
    }
}

static void
parse_expr (void)
{
  next ();
  while (tok.tag != '\0')
    {
      emit (tok);
      next ();
    }
}

static void
parse (const char *s) 
{
  start = s;
  scan = s + strlen (s);

  gen (HALT);
  parse_expr ();
  x86_prolog ();
}


int 
main (int argc, char **argv)
{
  i32 v;

  parse (argv[1]);
  if (0) dump_code ();
  v = run ();
  printf ("%d\n", v);
  
  return 0;
}
