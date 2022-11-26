from itertools import groupby
import numpy as np
import math
import sys

def process(file):
    with open(file) as f:
        time = int(f.readline().strip('\n'))
        bus_line = f.readline()
    result = findtime(bus_line)

    # allbuses = [parsebus(b) for b in bus_line.split(',')]
    # result = findphenomenaltime(allbuses)
    return result
    # for i in range(max(1, result - 100), result + 100):
    #     s = f"{i}\t"
    #     for b in allbuses:
    #         if b is None:
    #             continue
    #         if i % b == 0:
    #             s += "D\t"
    #         else:
    #             s += ".\t"
    #     print(s)


def findtime(bus_line):
    allbuses = [parsebus(b) for b in bus_line.split(',')]
    # this optimizes findphenomenaltime by increasing the increment each 
    # time we find a pattern.
    result = _findtime(allbuses, 0)[0]
    print(f"result: {result}")
    return result

def _findtime(allbuses, count):
    if len(allbuses) == 1:
        return allbuses[0], allbuses[0]
    if not allbuses[-1]:
        return _findtime(allbuses[:-1], count + 1)

    # first get the result of n-1
    prev_result, lcm = _findtime(allbuses[:-1], count + 1)
    cur = allbuses[-1]
    idx = len(allbuses)-1
    # then iterate by the least common multiple of prev.
    t = prev_result
    while True:
        if (t + idx) % cur == 0:
            return t, _lcm(lcm, cur)
        t += lcm

def _lcm(a, b):
    return abs(a*b) // math.gcd(a, b)

def all_equal(iterable):
    g = groupby(iterable)
    return next(g, True) and not next(g, False)

# def _lcm(a, b):
#     am = a
#     bm = b
#     while am != bm:
#         if am < bm:
#             am += a
#         if bm < am:
#             bm += b
#     print(f"_lcm({a}, {b}) = {am}")
#     return am

def findphenomenaltime(allbuses):
    # least frequent bus
    buses = [b for b in allbuses if b is not None]
    lfb = sorted(buses)[-1]
    lfbi = allbuses.index(lfb)
    lfbtime = 0

    # using bus 0
    lfb = allbuses[0]
    lfbi = 0

    while True:
        lfbtime += lfb
        # start at lfb and check right first.
        if lfbi < len(allbuses) - 1:
            i = lfbi + 1
            rightfound = True
            while i < len(allbuses):
                if (allbuses[i] is not None):
                    if (lfbtime - lfbi + i) % allbuses[i] != 0:
                        rightfound = False
                        break
                i += 1
            if not rightfound:
                continue

        if lfbi > 0:
            i = lfbi - 1
            leftfound = True
            while i >= 0:
                if (allbuses[i] is not None):
                    if (lfbtime - lfbi + i) % allbuses[i] != 0:
                        leftfound = False
                        break
                i -= 1
            if not leftfound:
                continue
                
        return lfbtime - lfbi


def parsebus(b):
    return None if b == 'x' else int(b)

if __name__ == "__main__":
    assert findtime("17,x,13,19") == 3417
    assert findtime("67,7,59,61") == 754018
    assert findtime("67,x,7,59,61") == 779210
    assert findtime("67,7,x,59,61") == 1261476
    assert findtime("1789,37,47,1889") == 1202161486

    result1 = process("sample.txt")
    assert result1 == 1068781
    # result = process("sample2.txt")
    result = process("input.txt")
    print(result)
