for n in range(2, 25):
# 比如n取24
    for x in range(2, n):
    # x取123456
        if n % x == 0:
        # 满足条件,break出循环,x+1,不执行else
        # n不变,如果if一直不满足,即未执行过break,即for中条件整个不满足,则else子句将会执行
            #print(n, 'equals', x, '*', n//x)
            break

    else:
        # 循环到底未找到一个因数
        print(n, 'is a prime number')
        
    for x in range(2, n):
    # x取123456

        if n % x == 0:
        # 只有满足条件的x才能被print出来
            print(n, 'equals', x, '*', n//x)