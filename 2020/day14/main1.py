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
                addr = m.group(1)
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
            mem[addr] = _mask(mask, val)
    return mem

def _mask(mask, val):
    b = list(format(val, '036b'))
    for i, m in enumerate(mask):
        if m != "X":
            b[i] = m
    return int(''.join(b), 2)

if __name__ == "__main__":
    # TODO: Make this take input as a flag

    result1 = process("s")
    assert result1 == 165
    # result = process("sample2.txt")
    result = process("in")
    print(result)
