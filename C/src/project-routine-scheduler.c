#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_SIZE 1024
#define FIRST_DIM 16          // 第一维固定为 1-16,就是学期总周数
#define MAX_STRING_LENGTH 100 // 每个字符串的最大长度

int
main (int argc, char *argv[])
{
  FILE *file = fopen (argv[1], "r");
  if (file == NULL)
    {
      perror ("无法打开文件");
      return EXIT_FAILURE;
    }

  // 统计 CSV 文件的行数和列数
  int rows = 0, cols = 0;
  char line[MAX_LINE_SIZE];
  while (fgets (line, sizeof (line), file))
    {
      if (rows == 0)
        {
          // 第一行确定列数
          char *token = strtok (line, ",");
          while (token != NULL)
            {
              cols++;
              token = strtok (NULL, ",");
            }
        }
      rows++;
    }

  // 重置文件指针到文件开头
  rewind (file);

  // 动态分配三维数组
  char ****data = malloc (FIRST_DIM * sizeof (char ***));
  for (int i = 0; i < FIRST_DIM; i++)
    {
      data[i] = malloc (rows * sizeof (char **));
      for (int j = 0; j < rows; j++)
        {
          data[i][j] = malloc (cols * sizeof (char *));
          for (int k = 0; k < cols; k++)
            {
              data[i][j][k] = malloc (MAX_STRING_LENGTH * sizeof (char));
            }
        }
    }

  // 读取 CSV 文件并填充三维数组
  int row = 0;
  int is_first_line = 1; // 用于跳过标题行
  while (fgets (line, sizeof (line), file))
    {
      line[strcspn (line, "\n")] = '\0'; // 去除换行符
      // 跳过标题行
      if (is_first_line)
        {
          is_first_line = 0;
          continue;
        }
      char *token = strtok (line, ",");
      int col = 0;
      while (token != NULL)
        {
          for (int i = 0; i < FIRST_DIM; i++)
            {
              // 将字符串复制到三维数组中
              snprintf (data[i][row][col], MAX_STRING_LENGTH, "%s-%d", token,
                        i + 1); // 示例：将第一维的值附加到字符串
            }
          token = strtok (NULL, ",");
          col++;
        }
      row++;
    }

  fclose (file);

  // 打印三维数组内容
  printf ("三维数组内容：\n");
  for (int i = 0; i < FIRST_DIM; i++)
    {
      printf ("第一维: %d\n", i + 1);
      for (int j = 0; j < rows; j++)
        {
          for (int k = 0; k < cols; k++)
            {
              printf ("%4s ", data[i][j][k]);
            }
          printf ("\n");
        }
      printf ("\n");
    }

  // 释放动态分配的内存
  for (int i = 0; i < FIRST_DIM; i++)
    {
      for (int j = 0; j < rows; j++)
        {
          for (int k = 0; k < cols; k++)
            {
              free (data[i][j][k]);
            }
          free (data[i][j]);
        }
      free (data[i]);
    }
  free (data);

  return EXIT_SUCCESS;
}