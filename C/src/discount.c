#include <stdio.h>

int
main ()
{
  int user_type;
  user_type = 2;
  float good_price;
  good_price = 180;
  float pay_price;
  if (user_type == 1)
    {
      if (good_price > 100)
        {
          pay_price = good_price * 0.95;
        }
      else
        {
          pay_price = good_price;
        };
    }
  else if (user_type == 2)
    {
      if (good_price > 200)
        {
          pay_price = good_price * 0.9;
        }
      else
        {
          pay_price = good_price * 0.97;
        };
    };
  printf ("最终支付金额为:%.2f\n", pay_price);
}