from itertools import groupby
import numpy as np
import math
from pprint import PrettyPrinter
import re
import sys
import time

def process(file):
    with open(file) as f:
        nextline = f.readline().strip('\n')

    return run(nextline)

def run(line):
    initial = [int(i) for i in line.split(',')]
    turns = {n:[i+1] for i,n in enumerate(initial)}
    # pp.pprint(turns)

    num = initial[-1]
    for turn in range(len(initial)+1,30000001):
        num = get_num(turns, num)
        set_turn(turns, num, turn)
        # if (turn % 1000000 == 0): print(turn)
    print(num)
    return num
        
def get_num(turns, prev_num):
    # pp.pprint(turns)
    # input("Press Enter to conitnue...")
    pt = turns[prev_num]
    assert len(pt) > 0
    if len(pt) == 1:
        return 0
    return pt[-1] - pt[-2]

def set_turn(turns, num, turn):
    if num not in turns:
        turns[num] = []
    turns[num].append(turn)

if __name__ == "__main__":
    # TODO: Make this take input as a flag
    pp = PrettyPrinter()

    # assert run("0,3,6") == 175594
    # assert run("1,3,2") == 2578
    # assert run("2,1,3") == 3544142
    # assert run("1,2,3") == 261214
    # assert run("2,3,1") == 6895259
    # assert run("3,2,1") == 18
    # assert run("3,1,2") == 362

    result = process("in")
    print(result)
