def proba (point,list_dis,list_weight,list_flag,alpha,beta):
    choose=random.random()
    list_p=[]
    for i in range (len(list_flag)):
        if list_flag[i]==0:
            list_dis[point][i]=0
    for i in range (len(list_flag)):
        if list_dis[point][i]==0:
            list_p.append(0)
        else:
            list_p.append((list_weight[point][i])**alpha*(1/list_dis[point][i])**beta)
    p_sum=0
    for i in range(len(list_p)):
        print(list_p[i])
        p_sum=p_sum+list_p[i]

    list_ls=[]
    for i in range(len(list_p)):
        p=0
        for j in range(0,i+1):
           p=p+list_p[j]
        list_ls.append(p/p_sum)
    for i in range(len(list_ls)):
        if choose<list_ls[i]:
            break
    nextpoint=i
    return nextpoint 