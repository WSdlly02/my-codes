#include <stdio.h>

int
main ()
{
  float profit, bonus;
  printf ("请输入利润:");
  scanf ("%f", &profit);
  if (profit < 10)
    {
      bonus = profit * 0.1;
      printf ("分红为%.2f\n", bonus);
    }
  else if (profit < 20)
    {
      bonus = 1 + (profit - 10) * 0.075;
      printf ("分红为%.2f\n", bonus);
    }
  else if (profit < 40)
    {
      bonus = 1 + 0.75 + (profit - 20) * 0.05;
      printf ("分红为%.2f\n", bonus);
    }
  else if (profit < 60)
    {
      bonus = 1 + 0.75 + 1 + (profit - 40) * 0.03;
      printf ("分红为%.2f\n", bonus);
    }
  else if (profit < 100)
    {
      bonus = 1 + 0.75 + 1 + 0.6 + (profit - 60) * 0.015;
      printf ("分红为%.2f\n", bonus);
    }
  else
    {
      bonus = 1 + 0.75 + 1 + 0.6 + 0.6 + (profit - 100) * 0.01;
      printf ("分红为%.2f\n", bonus);
    }
}