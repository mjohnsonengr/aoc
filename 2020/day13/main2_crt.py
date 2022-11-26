from functools import reduce
import math
import numpy as np
import os
import sys

def process(file):
    with open(file) as f:
        time = int(f.readline().strip('\n'))
        bus_line = f.readline()

    result = int(findtime(bus_line))

    allbuses = [parsebus(b) for b in bus_line.split(',')]
    # for i in range(max(1, result - 10), result + 100):
    #     s = f"{i}\t"
    #     for b in allbuses:
    #         if b is None:
    #             continue
    #         if i % b == 0:
    #             s += "D\t"
    #         else:
    #             s += ".\t"
    #     print(s)

    return result

def findtime(bus_line):
    allbuses = [parsebus(b) for b in bus_line.split(',')]
    realbuses = [(i, b) for i, b in enumerate(allbuses) if b]
    # could sort realbuses by [1] here
    print(allbuses)
    print(realbuses)
    n = [b for i,b in realbuses]
    print(n)
    a = [wrap(b - i, 0, b) for i,b in realbuses]
    print(a)
    result = chinese_remainder(n,a)
    
    for i, b in enumerate(allbuses):
        if not b:
            continue
        assert (result + i) % b == 0
    return result

def wrap(val, l, h):
  diff = h - l
  while val >= h:
    val -= diff
  while val < l:
    val += diff
  return val

def parsebus(b):
    return None if b == 'x' else int(b)

# https://fangya.medium.com/chinese-remainder-theorem-with-python-a483de81fbb8
def chinese_remainder(n, a):
    # For some reason, this works for _every_ sample, but not the real input.
    # I _think_ we can rule big numbers out as a cause because this is Python.
    sum=0
    prod=reduce(lambda a, b: a*b, n)
    for n_i, a_i in zip(n,a):
        p=prod/n_i
        sum += a_i* mul_inv(p, n_i)*p
    return sum % prod
def mul_inv(a, b):
    b0= b
    x0, x1= 0,1
    if b== 1: return 1
    while a>1 :
        q=a// b
        a, b= b, a%b
        x0, x1=x1 -q *x0, x0
    if x1<0 : x1+= b0
    return x1

def chdir_scriptdir():
    abspath = os.path.abspath(sys.argv[0])
    dname = os.path.dirname(abspath)
    os.chdir(dname)

if __name__ == "__main__":
    chdir_scriptdir()

    print(findtime("17,x,13,19"))
    assert findtime("17,x,13,19") == 3417
    assert findtime("67,7,59,61") == 754018
    assert findtime("67,x,7,59,61") == 779210
    assert findtime("67,7,x,59,61") == 1261476
    assert findtime("1789,37,47,1889") == 1202161486

    # result = process("sample.txt")
    # result = process("sample2.txt")
    result = process("input.txt")
    print(result)