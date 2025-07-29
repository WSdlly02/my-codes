#include <stdio.h>
#include <stdlib.h>

int
fib (int i, int n, int m)
{
  if (i == 0)
    return n;
  else
    return fib ((i - 1), m, (n + m));
}

int
main (int argc, char *argv[])
{
  printf ("%d\n", argc);
  printf ("%ld\n", (sizeof argv[7])); // pointer's size
  // printf ("%c\n", *argv[30]);
  for (int i = 0; argv[i] != NULL; i++)
    {
      printf ("%s ", argv[i]);
    }
  // printf ("%p ", argv[3]);        // pointer
  // printf ("%s ", argv[7]);        // translate address to string
  // printf ("%d ", *argv[3]);       // the first letter in ASCII
  // printf ("%c ", *argv[3]);       // the first letter
  // printf ("%c ", *(argv[0] + 1)); // the second letter
  printf ("\n");
  if (argc == 1)
    {
      printf ("请输入参数!\n");
      return 1;
    }
  int step = atoi (argv[1]);
  printf ("第%d阶斐波那契数为%d\n", step, fib (step, 0, 1));
}