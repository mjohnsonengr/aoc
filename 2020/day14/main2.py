from itertools import groupby
import numpy as np
import math
import re
import sys

def process(file):
    with open(file) as f:
        nextline = f.readline().strip('\n')
        ops = []
        while nextline != "":
            mask = nextline.split()[2]
            nextline = f.readline().strip('\n')
            instr = []
            while nextline.startswith("mem"):
                m = re.match(r"mem\[([0-9]+)\] = ([0-9]+)", nextline)
                addr = int(m.group(1))
                val = int(m.group(2))
                instr.append((addr, val))
                nextline = f.readline().strip('\n')
            ops.append((mask, instr))


    result = sim(ops)
    return sum(result.values())

def sim(ops):
    mem = {}
    for mask, instr in ops:
        for addr, val in instr:
            addrs = _mask(mask, addr)
            for addr in addrs:
                mem[addr] = val
    return mem

def _mask(mask, val):
    b = list(format(val, '036b'))
    xloc = []
    for i, m in enumerate(mask):
        if m == "0":
            continue
        if m == "1":
            b[i] = "1"
        if m == "X":
            xloc.append(i)
            b[i] = "X"
    # b is the mem addr result
    xn = len(xloc)
    al = []
    for i in range(2**xn):
        al.append(format(i, f"0{xn}b"))

    results = []
    for a in al:
        # a is e.g. "01"
        for i, c in enumerate(a):
            b[xloc[i]] = c
        print(b)
        results.append(int(''.join(b)))
    return results
        

if __name__ == "__main__":
    # TODO: Make this take input as a flag

    # result1 = process("s")
    # assert result1 == 165
    result2 = process("s2")
    print(result2)
    assert result2 == 208
    # result = process("sample2.txt")
    # result = process("in")
    print(result)
