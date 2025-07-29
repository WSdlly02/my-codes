#include <stdbool.h>
#include <stdio.h>

int
main ()
{
  int a = 2;
  int b = 8;
  int diff;
  if (a > b)
    {
      diff = a - b;
    }
  else
    {
      diff = b - a;
    };
  printf ("a与b的差值为:%d\n", diff);
}