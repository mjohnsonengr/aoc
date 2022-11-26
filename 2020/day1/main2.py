with open('input.txt') as f:
    l =  int(i)for liin f.readlines()]
s = set(l)
s2 = {}
# s2 is map keyed by sum with tuple of numbers
for i in l:
    for j in l:
        s2[i+j] = (i,j)
for i in l:
    if (2020-i) in s2:
        e = s2[2020-i]
        print(i * e[0] * e[1])
        exit()