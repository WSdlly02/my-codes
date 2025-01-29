#include <stdio.h>

int
main ()
{
  int good_category;
  float good_price;
  good_category = 2;
  good_price = 299.9;
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
      return 1;
      break;
    };
}