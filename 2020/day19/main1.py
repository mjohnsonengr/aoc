from itertools import groupby
import numpy as np
import math
from pprint import PrettyPrinter
import re
import sys
import time


def process(file):
    sum = 0
    with open(file) as f:
        line = f.readline().rstrip()
        # read rules
        rules = {}
        while line:
            col = line.index(":")
            num = line[:col]
            seqs = [seqstr.split(" ") for seqstr in line[col+2:].split(" | ")]
            rules[num] = seqs
            line = f.readline().rstrip()

        print(rules)
        print(line)
        line = f.readline().rstrip()
        while line:
            v, i = valid(line, rules)
            if v:
                print(f"{line} is valid")
                sum += 1
            line = f.readline().rstrip()

    print(sum)
    return sum

def valid(line, rules, rule = "0", i = 0):
    if i >= len(line):
        return False, i
    if rules[rule] == [['"a"']]:
        return line[i] == "a", i+1
    if rules[rule] == [['"b"']]:
        return line[i] == "b", i+1

    for seq in rules[rule]:
        v, new_i = seq_valid(line, rules, seq, i)
        if v:
            return v, new_i
    return False, i
                
def seq_valid(line, rules, seq, i):
    for subrule in seq:
        v, i = valid(line, rules, subrule, i)
        if not v:
            return False, i
    return True, i


if __name__ == "__main__":
    # TODO: Make this take input as a flag
    pp = PrettyPrinter()

    assert process("smp") == 2

    time.sleep(1)
    result = process("in")
    print(result)
