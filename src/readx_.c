#include <stdio.h>

int readx_ ( char *s , int l )
{
  char     c[255]  ;
  register char *b ;
  do { if ( feof ( stdin ) ) return 1; } while ( gets(c) == NULL ) ;
  for ( b = c ; l > 0 ; l-- ) *s++ = ( *b != '\0' ) ? *b++ : ' ' ;
}
