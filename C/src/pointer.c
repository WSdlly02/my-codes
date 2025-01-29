#include <stdio.h>

int
main ()
{
  char ch = 'A';
  char *ptr_ch = &ch;
  *ptr_ch = 'a';
  printf ("此时ch为%c\n", ch);
}