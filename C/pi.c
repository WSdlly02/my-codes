#include <stdio.h>
#include <math.h>

const float PI = 3.14;
int main() { 
    int r_1 = 2;
    int r_2 = 8;
    printf("圆r_1面积为:%.2f\n圆r_2面积为:%.2f",PI*pow(r_1,2),PI*pow(r_2,2));
}