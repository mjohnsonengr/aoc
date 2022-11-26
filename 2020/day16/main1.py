from itertools import groupby
import numpy as np
import math
from pprint import PrettyPrinter
import re
import sys
import time

def process(file):
    fields = {}
    myticket = []
    tickets = []
    with open(file) as f:
        nextline = f.readline().rstrip()
        while (nextline):
            match = re.match(r"([a-z\s]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)", nextline)
            field = match.group(1)
            low1 = int(match.group(2))
            high1 = int(match.group(3))
            low2 = int(match.group(4))
            high2 = int(match.group(5))
            fields[field] = set(range(low1, high1+1)).union(set(range(low2, high2+1)))
            nextline = f.readline().rstrip()


        nextline = f.readline().rstrip()
        assert nextline == "your ticket:"
        nextline = f.readline().rstrip()
        myticket = [int(i) for i in nextline.split(',')]
        # tickets.append(myticket)
        nextline = f.readline().rstrip() # empty

        nextline = f.readline().rstrip()
        assert nextline == "nearby tickets:"

        nextline = f.readline().rstrip()
        while (nextline):
            tickets.append([int(i) for i in nextline.split(',')])
            nextline = f.readline().rstrip()

    return get_invalid(fields, tickets)

def get_invalid(fields, tickets):
    valid_values = set().union(*fields.values())
    # old approach:
    # valid_values = set()
    # for field in fields.values():
    #     for low, high in field:
    #         valid_values = valid_values.union(set(range(low, high+1)))

    sum = 0
    for ticket in tickets:
        for val in ticket:
            if val not in valid_values:
                sum += val
    return sum

if __name__ == "__main__":
    # TODO: Make this take input as a flag
    pp = PrettyPrinter()

    assert process("smp") == 71

    time.sleep(1)
    result = process("in")
    print(result)
