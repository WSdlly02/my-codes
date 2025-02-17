#include <stdio.h>

int
main ()
{
  int i, j, result;
  for (i = 1; i < 10; i++)
    {
      for (j = 1; j <= i; j++)
        {
          result = i * j;
          printf ("%dx%d=%-4d", i, j, result); // %-4d表示左对齐，占4位
        }
      printf ("\n");
    }
}