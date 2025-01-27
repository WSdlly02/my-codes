#include <stdio.h>

int main() {
    int mike_score = 89;
    int john_score = 98;
    mike_score = john_score;
    john_score = 89;
    printf("mike的成绩是%d,john的成绩是%d\n",mike_score,john_score);
}