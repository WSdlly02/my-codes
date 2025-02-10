#include <stdio.h>

int
main ()
{
  const int num_dict[4] = { 1, 2, 3, 4 };
  printf ("有%d个这样的三位数\n", 4 * 3 * 2);
  int g, s, b, counter = 0;
  for (b = 1; b < 5; b++)
    {
      for (s = 1; s < 5; s++)
        {
          for (g = 1; g < 5; g++)
            {
              if (g != b && g != s && b != s)
                {
                  printf ("数字为%d\n", g + s * 10 + b * 100);
                  counter++;
                }
            }
        }
    }
  printf ("%d\n", counter);
}