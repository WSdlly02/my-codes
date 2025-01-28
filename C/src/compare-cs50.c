#include <cs50.h>
#include <stdio.h>

int main(void) {
    int x = get_int("What's x? ");
    int y = get_int("What's y? ");

    if (x<y)
    {
        printf("x:%i is less than y:%i\n",x,y);
    }
    else if (x>y)
    {
        printf("x:%i is greater than y:%i\n",x,y);
    }
    else // if (x==y)
    {
        printf("x is equal to y:%i\n",x);
    };
}