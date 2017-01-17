#include <stdio.h>
#include <stdlib.h>

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

#include "cisc.h"

int main()
{
  START_MACHINE;

  JUMP(CONTINUE);

#include "char.lib"
#include "io.lib"
#include "math.lib"
#include "string.lib"
#include "system.lib"

 CONTINUE:
SOB_FALSE
SOB_TRUE
 STOP_MACHINE;

  return 0;
}