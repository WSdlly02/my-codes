import math

x = math.pow(1 + math.pi, 1 / 3)
y = ((math.e**-x) - math.tan(73 / 180 * math.pi)) / (
    10**-5 + math.log(abs(math.sin(x) ** 2 - math.sin(x**2)))
)
print(y)
