#include <stdio.h>

int
main ()
{
  int i, x, y, z;

  // 遍历 100 到 999 的数字
  for (i = 100; i < 1000; i++)
    {
      // 计算各位数字
      x = i % 10;         // 个位
      y = (i / 10) % 10;  // 十位
      z = (i / 100) % 10; // 百位

      // 计算立方和
      int sum = x * x * x + y * y * y + z * z * z;

      if (i == sum)
        {
          printf ("%d\n", i);
        }
    }
}