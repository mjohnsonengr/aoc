with open('input.txt') as f:
    l = [int(i) for i in f.readlines()]
s = set(l)
for i in l:
    if (2020-i) in s:
        print(i * (2020-i))
        exit()