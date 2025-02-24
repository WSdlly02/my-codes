#include <stdio.h>

int
main ()
{
  char buf[100];
  gets (buf); // C11 起被移除，需改用 fgets()
  return 0;
}