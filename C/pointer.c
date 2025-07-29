#include <stdio.h>

int
main ()
{
  char ch = 'A';      // 定义一个变量指向A,本质上它是一个编译期指针
  char *ptr_ch = &ch; // 定义一个指针,定义中的*代表指针变量,它取变量ch的地址
  *ptr_ch
      = 'a'; // 此时ptr_ch指向一个内存地址,*代表解引用,把内存地址还原为它存储的内容
  printf ("此时ch为%c\n地址为%p\n", *ptr_ch, ptr_ch);
}