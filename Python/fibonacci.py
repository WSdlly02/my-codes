import sys


def fib(i: int, n: int, m: int):
    if i == 0:
        return n
    else:
        return fib(i=(i - 1), n=m, m=(n + m))


step = int(sys.argv[1])
print(f"the {step}th fibonacci number is {fib(i=step,n=0,m=1)}")
