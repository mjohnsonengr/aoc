from itertools import groupby
import math
import numpy as np
import os
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

        line = f.readline().rstrip()
        while line:
            v = valid(line, rules)
            if v:
                print(f"{line}")
                sum += 1
            line = f.readline().rstrip()

    print(sum)
    return sum

def valid(line, rules):
    rules = dict(rules)
    # we know the first rule is 0: 8 11
    # we know 8 is 42 | 42 8
    # we know 11 is 42 31 | 42 11 31
    rules["8"] = [["42"]]
    rules["11"] = [["42", "31"], ["42", "11", "31"]]
    # keep adding a 42 to rule 8 until there are too many to possibly work
    while len(rules["8"][0]) < len(line):
        v, i = _valid(line, rules, "0", 0)
        if v and i == len(line):
            return True
        rules["8"][0].append("42")
    return False


def _valid(line, rules, rule, i):
    # print(rule)
    if i >= len(line):
        # print(f"rule {rule} False because i too large")
        return False, i
    if rules[rule] == [['"a"']]:
        v = line[i] == "a"
        # print(f"rule {rule} {v} for i {i}")
        return line[i] == "a", i+1
    if rules[rule] == [['"b"']]:
        v = line[i] == "b"
        # print(f"rule {rule} {v} for i {i}")
        return line[i] == "b", i+1

    for seq in rules[rule]:
        v, new_i = seq_valid(line, rules, seq, i)
        if v:
            # print(f"rule {rule} True")
            return v, new_i
    # print(f"rule {rule} False")
    return False, i
                
def seq_valid(line, rules, seq, i):
    for subrule in seq:
        v, i = _valid(line, rules, subrule, i)
        if not v:
            return False, i
    return True, i


def chdir_scriptdir():
    abspath = os.path.abspath(sys.argv[0])
    dname = os.path.dirname(abspath)
    os.chdir(dname)


if __name__ == "__main__":
    chdir_scriptdir()
    # TODO: Make this take input as a flag
    pp = PrettyPrinter()

    assert process("smp3") == 1

    assert process("smp2") == 12

    time.sleep(1)
    result = process("in")
    print(result)
