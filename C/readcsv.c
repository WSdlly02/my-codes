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
  if (argc == 1)
    {
      printf ("用法: %s <CSV文件路径>\n", argv[0]);
      return EXIT_FAILURE;
    }

  char line[MAX_LINE_SIZE];
  int is_first_line = 1; // 用于跳过标题行
  int weeks = 0;
  while (weeks < 16)
    {
      weeks = weeks + 1;

      FILE *file = fopen (argv[1], "r");

      if (file == NULL)
        {
          perror ("");
          return EXIT_FAILURE;
        }

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
          char *class_time = strtok (line, ",");
          char *monday = strtok (NULL, ",");
          char *tuesday = strtok (NULL, ",");
          char *wednesday = strtok (NULL, ",");
          char *thursday = strtok (NULL, ",");
          char *friday = strtok (NULL, ",");

          // 按照指定格式输出

          if (class_time != NULL)
            {
              printf ("当前周数: %d, 上课时间: %s, 星期一: %s, 星期二: %s\n",
                      weeks, class_time, monday, tuesday);
            }
        }
      fclose (file);
    }
  return EXIT_SUCCESS;
}