#include <stdio.h>
#include <string.h>

int
main ()
{
  int step, i;
  printf ("输入阶数:");
  if (scanf ("%d", &step))
    ;

  char triangle[3 * step];
  memset (triangle, '\0', sizeof (triangle));

  for (i = 1; i <= step - 1; i++) // n-1
    strcat (triangle, " ");
  char stars[2 * step];
  memset (stars, '*', sizeof (stars) - 1); // 2n-1
  stars[2 * step] = '\0';
  strcat (triangle, stars);

  for (i = 0; i < step; i++)
    printf ("%.*s\n", step + i, triangle + i);
}