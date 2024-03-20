#include <time.h>

#ifndef  CLOCKS_PER_SEC
#define  CLOCKS_PER_SEC 1000000
#endif

float clock_()
{
  return clock() / (float)CLOCKS_PER_SEC;
}
