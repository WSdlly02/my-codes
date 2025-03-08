factorial 0 = 1
factorial n = n * factorial (n-1)

accumMulti = map factorial [1..20]
