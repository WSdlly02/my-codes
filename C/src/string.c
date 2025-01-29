#include <stdio.h>

int
main ()
{
  int user_sum;
  char name[16];
  printf ("输入打招呼总人数:");
  scanf ("%d", &user_sum);
  for (int i = 0; i < user_sum; i++)
    {
      printf ("请输入用户名:");
      scanf ("%15s", name);
      // 字符串数组不用添加取地址符
      printf ("%s,你好\n", name);
    };
  printf ("你已打完所有招呼!\n");
}