import sys
import math

a = -1
b = -2
c = 3
root_1 = (-b + math.sqrt(b**2 - 4 * a * c)) / (2 * a)
root_2 = (-b - math.sqrt(b**2 - 4 * a * c)) / (2 * a)
print(root_1, root_2)
