#include <stdio.h>

int
main ()
{
  int year, month, day, sum = 0;
  int daysInMonth[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
  printf ("请输入年,月,日:");
  scanf ("%d,%d,%d", &year, &month, &day);
  if ((year % 4 == 0 && year % 100 != 0) || (year % 400 == 0))
    {
      daysInMonth[1] = 29;
      for (int i = 1; i < month; i++)
        {
          sum = sum + daysInMonth[i];
        }
    }
  else
    {
      for (int i = 1; i < month; i++)
        {
          sum = sum + daysInMonth[i];
        }
    }
  sum = sum + day;
  printf ("%d年%d月%d日是该年的第%d天\n", year, month, day, sum);
}