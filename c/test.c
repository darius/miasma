#include <stdio.h>

#include "asm.h"


static u8 buf[64];
static x86_bufptr x86_bptr = buf + sizeof buf;
static i32 x86_tmp;		/* ugh */

typedef int (*func)();

static func test_func;


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
  x86_push_gv (edi);
  x86_push_gv (esi);
  x86_push_gv (ebx);
  x86_mov_gv_ev (ebp, reg (esp));
  x86_push_gv (ebp);
}

static void
whump (void)
{
  x86_ret ();
  x86_epilog ();
  x86_add_gv_ev (eax, atv (ebp, 8));
  x86_mov_gv_iv (eax, 42);
  x86_prolog ();

  test_func = (func) x86_bptr;
}


static void
dump_code (void)
{
  x86_bufptr bp = x86_bptr;
  for (; bp < buf + sizeof buf; ++bp)
    printf ("%02x ", *bp);
  printf ("\n");
}

int
main ()
{
  whump ();
  dump_code ();
  printf ("%d\n", test_func (8));
  return 0;
}
