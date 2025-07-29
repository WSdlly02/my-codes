#include <stdio.h>

int
main ()
{
  float c, f;
  c = 18;
  f = 9.0 / 5 * c + 32;
  int i = 1;
  i = (++i) + (++i);
  int a = 12;
  a += a -= a * a;
  printf ("%d\n%d\n", a, i);
  printf ("%f,%ld,%ld", f, sizeof (f), sizeof (c));
}