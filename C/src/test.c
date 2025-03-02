#include <stdio.h>

int
main ()
{
  int day, weekday;
  if (scanf ("%d", &day))
    {
      weekday = (day - 1) % 7;
      char *weekday_cond[7] = { "三", "四", "五", "六", "日", "一", "二" };
      printf ("%s\n", weekday_cond[weekday]);
    }
}
