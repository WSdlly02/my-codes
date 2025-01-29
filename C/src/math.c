#include <math.h>
#include <stdio.h>

int result_1 = 2 + 10 / 8 * 3;
double result_2 = 2 + 10 / 8.0 * 3;
int
main ()
{
  printf ("9的平方根是%lf\n", sqrt (9));
  printf ("result_1:%d\nresult_2:%lf\n", result_1, result_2);
  int a = -1;
  int b = -2;
  int c = 3;
  double x_1 = (-b + sqrt (pow (b, 2) - 4 * a * c)) / 2 * a;
  double x_2 = (-b - sqrt (pow (b, 2) - 4 * a * c)) / 2 * a;
  printf ("x_1:%lf\nx_2:%lf\n", x_1, x_2);
}