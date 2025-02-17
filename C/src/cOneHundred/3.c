#include <math.h>
#include <stdio.h>

int
main ()
{
  int m, n, i, j;
  for (i = 2; i <= 84; i = i + 2)
    {
      if (168 % i == 0)
        {
          j = 168 / i;
          m = (i + j) / 2;
          n = (i - j) / 2;
          if ((pow (m, 2) - pow (n, 2)) == 168)
            {
              printf ("x为%.0f\n", pow (n, 2) - 100);
            }
        }
    }
}
