unsigned ishft_( unsigned *pattern, int *value)
{
  int ival;
  if( *value > 0 ) {
    ival = *value & 31;
    return *pattern << ival;
  } else if( *value < 0 ) {
    ival = (- *value) & 31;
    return *pattern >> ival;
  }
  return *pattern;
}
