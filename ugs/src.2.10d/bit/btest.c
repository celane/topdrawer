unsigned btest_( unsigned *pattern, unsigned *bit)
{
  int ibit = *bit & 31;
  return (*pattern & ( 1 << ibit )) ? 1 : 0;
}
