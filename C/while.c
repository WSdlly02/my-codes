#include <stdio.h>

int num;
int num_accum;

void
echo ()
{
  printf ("1到100所有偶数的和为:%d\n", num_accum);
}

int
main ()
{
  printf ("num:%d\nnum_accum:%d\n", num, num_accum);
  //
  num = 1;
  num_accum = 0;
  while (num <= 100)
    {
      num = num + 1;
      if (num % 2 == 0)
        {
          num_accum = num_accum + num;
        };
    };
  echo ();
  //
  num = 1;
  num_accum = 0;
  do
    {
      num = num + 1;
      if (num % 2 == 0)
        {
          num_accum = num_accum + num;
        };
    }
  while (num <= 100);
  echo ();
}