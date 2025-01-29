#include <stdio.h>

int
main ()
{
  int good_category;
  float good_price;
  printf ("请输入商品类别:");
  scanf ("%d", &good_category);
  printf ("请输入商品价格:");
  scanf ("%f", &good_price);
  switch (good_category)
    {
    case 1:
      if (good_price < 500)
        {
          printf ("电子产品价格较低,无优惠\n");
        }
      else if (good_price <= 1000)
        {
          printf ("电子产品可享受5\%的优惠\n");
        }
      else
        {
          printf ("电子产品可享受10\%的优惠\n");
        };
      break;
    case 2:
      if (good_price < 200)
        {
          printf ("服装价格较低,无优惠\n");
        }
      else if (good_price <= 500)
        {
          printf ("服装可享受8\%的优惠\n");
        }
      else
        {
          printf ("服装可享受15\%的优惠\n");
        };
      break;
    default:
      printf ("格式错误!\n");
      return 1;
      break;
    };
}