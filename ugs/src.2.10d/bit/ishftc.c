unsigned ishftc_( unsigned *pattern, int *value, int *bits)
{
  int ival, ibit, jbit;
  unsigned upr, lwr;
  ibit = *bits & 31;
  jbit = 32-ibit;
  upr  = (*pattern >> ibit) << ibit;
  lwr  = (*pattern << jbit) >> jbit;
  if( *value == 0 ) 
    return *pattern;
  else if( *value > 0 ) {
    ival = *value & 31;
    lwr  = lwr << ival;
  } else if( *value < 0 ) {
    ival = (- *value) & 31;
    lwr  = lwr >> ival;
  }
  return (upr | ((lwr << jbit) >> jbit) | (lwr >> ibit));
}
