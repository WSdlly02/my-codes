import sys


def fib(i, n, m):
    if i == 0:
        return n
    else:
        return fib((i - 1), m, (n + m))


step = int(sys.argv[1])
print(f"the {step}th fibonacci number is {fib(step,0,1)}")