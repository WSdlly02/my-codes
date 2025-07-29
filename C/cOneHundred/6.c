#include <stdio.h>

int
main ()
{
  // 定义字符图形
  const char *c_pattern[] = { " ****", " *", " *", " ****" };

  // 输出字符图形
  printf ("用 * 号输出字母 C!\n");
  for (
      int i = 0; i < sizeof (c_pattern) / sizeof (c_pattern[0]);
      i++) // sizeof函数返回一个对象或类型所占的内存字节数,总内存字节数除以单个数组元素的字节数等于数组中的元素个数
    {
      printf ("%s\n", c_pattern[i]);
    }

  return 0;
}