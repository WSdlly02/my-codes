#include <stdio.h>

int
main ()
{
  int num, num_cache, i;
  if (scanf ("%d", &num))
    {
      num_cache = num;
      for (i = 0; i < num; ++i)
        {
          if (num_cache == 0)
            break;
          num_cache = num_cache / 10;
        }
      printf ("%d", i);
    }
}