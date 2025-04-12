def fib(n):    # write Fibonacci series less than n

    """Print a Fibonacci series less than n."""

    a, b = 0, 1

    while a < n:

        print(a, end=' ')

        a, b = b, a+b

    print() # 用于输出换行符