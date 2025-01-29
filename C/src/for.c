#include <stdio.h>

int num;

int
main ()
{
  for (num = 1; num <= 50; num++)
    {
      if ((num % 2 == 0) && (num % 3 != 0))
        {
          if (num > 40)
            {
              break;
            };
          printf ("数字为:%d\n", num);
        }
      else
        {
          continue; // 可以省略
        };
    };
}