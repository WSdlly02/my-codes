#include <stdio.h>

int
main ()
{
  int a, b, c, x, y, z;

  a = 11;
  b = 22;
  c = 33;

  if (a > b && a > c)
    {
      printf ("%d 最大", a);
      if (b > c)
        {
          x = a;
          y = c;
          z = b;
          printf ("%d>%d>%d", x, y, z);
        }
      else
        {
          x = a;
          y = b;
          z = c;
          printf ("%d>%d>%d", x, y, z);
        }
    }
  else if (b > a && b > c)
    {
      printf ("%d 最大", b);
      if (a > c)
        {
          x = b;
          y = a;
          z = c;
          printf ("%d>%d>%d", x, y, z);
        }
      else
        {
          x = b;
          y = c;
          z = a;
          printf ("%d>%d>%d", x, y, z);
        }
    }
  else if (c > a && c > b)
    {
      printf ("%d 最大", c);
      if (b > a)
        {
          x = c;
          y = b;
          z = a;
          printf ("%d>%d>%d", x, y, z);
        }
      else
        {
          x = c;
          y = a;
          z = b;
          printf ("%d>%d>%d", x, y, z);
        }
    }
  else
    printf ("有两个或三个数值相等");

  return 0;
}