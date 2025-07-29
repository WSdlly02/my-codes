#include <stdio.h>

void
echo (void)
{
  printf ("Hello!\n");
}

int
main (void)
{
  int i = 0;
  while (i < 3)
    {
      printf ("Hello! i is %i now!\n", i);
      i += 1; // i = i+1; || i++;
    };
  for (int i = 0; i < 3; i++)
    {
      echo ();
      printf ("i is %i now!\n", i);
    };
}