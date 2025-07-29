#include <stdio.h>
#include <unistd.h>

int
main ()
{
  int arr[5];
  for (int i = 0; i < 5; i++)
    // 0->4共5个数字,等于数组大小
    {
      printf ("请输入数组第%d个值:", i + 1);
      scanf ("%d", &arr[i]);
    };
  // scanf ("%d,%d", &arr[0], &arr[1]);
  for (int i = 0; i < 5; i++)
    // 循环在i = 5时不满足i < 5条件表达式时停止
    // 由于i递增,也可以将条件表达式写作i != 5
    {
      printf ("该数组的第%d个元素为%d\n", i + 1, arr[i]);
      usleep (100 * 1000); // 睡眠100毫秒
    };
}