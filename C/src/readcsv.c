#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_SIZE 1024

int
main (int argc, char *argv[])
/*
*argv[]是指针数组,存储字符串
使用指针(char *argv[])可以动态分配内存,只占用实际需要的空间
指针可以指向任意长度的字符串,而不需要提前知道字符串的具体大小
注意: argv[argc] = NULL
*/
{
  printf ("args:%d\n", argc);
  if (argc == 1)
    {
      printf ("用法: %s <CSV文件路径>\n", argv[0]);
      return EXIT_FAILURE;
    }

  FILE *file = fopen (argv[1], "r");

  if (file == NULL)
    {
      perror ("无法打开文件");
      return EXIT_FAILURE;
    }

  char line[MAX_LINE_SIZE];
  int is_first_line = 1; // 用于跳过标题行

  while (fgets (line, sizeof (line), file))
    {
      // 去除行末的换行符
      line[strcspn (line, "\n")] = '\0';

      // 跳过标题行
      if (is_first_line)
        {
          is_first_line = 0;
          continue;
        }

      // 使用 strtok 按逗号分割行
      char *name = strtok (line, ",");
      char *age = strtok (NULL, ",");
      char *city = strtok (NULL, ",");

      // 按照指定格式输出
      if (name != NULL && age != NULL)
        {
          printf ("Name: %s, Age: %s, City: %s\n", name, age, city);
        }
    }

  fclose (file);
  return EXIT_SUCCESS;
}