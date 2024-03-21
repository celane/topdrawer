#include <stdlib.h>
#include "f2c.h"

int exit_(status)
integer *status;
{
  exit(*status);
}
