#include <stdio.h>
#include <stdlib.h>

int
fib (int i, int n, int m)
{
  if (i == 0)
    {
      return n;
    }
  else
    {
      return fib ((i - 1), m, (n + m));
    }
} // inspired from Nix

int
main (int argc, char *argv[])
{
  printf ("%p\n", argv[1]);
  if (argc == 1)
    {
      printf ("请输入参数!\n");
      return 1;
    }
  int step = atoi (argv[1]);
  for (int i = 1; i <= step; i++)
    {
      printf ("第%d阶斐波那契数为%d\n", i, fib (i, 0, 1));
    }
}