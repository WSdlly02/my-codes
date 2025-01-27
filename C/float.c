#include <stdio.h>

int main() {
    float float_1 = 11.27;
    float float_2 = 11.12345678910;
    printf("float_1:%.2f\nfloat_2:%f\n",float_1,float_2);
    double double_1 = 11.12345678910;
    printf("float_2:%.10f\ndouble_2:%.10lf\n",float_2,double_1);
}