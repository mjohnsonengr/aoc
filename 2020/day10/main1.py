import re
from collections import deque

def process(file):
    with open(file) as f:
        l = [int(i) for i in f]

    num1 = 0
    num3 = 1
    j = 0
    l.sort()
    print(l)
    for i in l:
        diff = i - j
        if diff == 1: num1 += 1
        if diff == 3: num3 += 1
        print((j, diff, i))
        j = i

    print((num1, num3))
    return num1*num3



result = process("sample.txt")
# result = process("input.txt")
# result = process("sample2.txt")
print(result)
