#include <iostream>

int
main ()
{
  int x, y;                         // 声明变量
  std::cin >> x >> y;               // 读入 x 和 y
  std::cout << y << std::endl << x; // 输出 y,换行,再输出 x
  return 0;                         // 结束主函数
}