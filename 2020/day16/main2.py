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
            match = re.match(
                r"([a-z\s]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)", nextline)
            field = match.group(1)
            low1 = int(match.group(2))
            high1 = int(match.group(3))
            low2 = int(match.group(4))
            high2 = int(match.group(5))
            fields[field] = set(range(low1, high1+1)
                                ).union(set(range(low2, high2+1)))
            nextline = f.readline().rstrip()

        nextline = f.readline().rstrip()
        assert nextline == "your ticket:"
        nextline = f.readline().rstrip()
        myticket = [int(i) for i in nextline.split(',')]
        # tickets.append(myticket)
        nextline = f.readline().rstrip()  # empty

        nextline = f.readline().rstrip()
        assert nextline == "nearby tickets:"

        nextline = f.readline().rstrip()
        while (nextline):
            tickets.append([int(i) for i in nextline.split(',')])
            nextline = f.readline().rstrip()

    tickets = discard_invalid(fields, tickets)
    field_pos = determine_field_pos(fields, tickets)
    return result(myticket, field_pos)


def discard_invalid(fields, tickets):
    valid_values = set().union(*fields.values())

    new_tickets = []
    for ticket in tickets:
        valid = True
        for val in ticket:
            if val not in valid_values:
                valid = False
        if valid:
            new_tickets.append(ticket)
    return new_tickets


def determine_field_pos(fields, tickets):
    """ Return map e.g. [field: pos] """
    num_fields = len(fields)
    allpos = set(range(num_fields))
    npfp = {field: [] for field in fields}  # list of fields NOT possible
    for ticket in tickets:
        for i, val in enumerate(ticket):
            for field in fields:
                if not val in fields[field]:
                    npfp[field].append(i)

    pfp = {field: allpos.difference(npfp[field]) for field in fields}
    field_pos = {}

    spfp = sorted(
        [(field, pfp[field]) for field in fields if field not in field_pos],
        key=lambda f: len(f[1]))
    # find fields with only on possible position
    while len(spfp) > 0 and len(spfp[0][1]) == 1:
        for field, pos in spfp:
            assert len(pos) != 0
            if len(pos) == 1:
                (pos,) = pos
                field_pos[field] = pos
                # Remove this pos from other possibilities.
                for field in fields:
                    if pos in pfp[field]:
                        pfp[field].remove(pos)
        spfp = sorted(
            [(field, pfp[field]) for field in fields if field not in field_pos],
            key=lambda f: len(f[1]))
    print('='*15)
    pp.pprint(spfp)

    # maybe also need to find positions with only one possible field?
    assert len(field_pos) == num_fields

    return field_pos


def result(myticket, field_pos):
    product = 1
    for field in field_pos:
        if "departure" in field:
            product *= myticket[field_pos[field]]
    return product


if __name__ == "__main__":
    # TODO: Make this take input as a flag
    pp = PrettyPrinter()

    if process("smp") == 1:
        print("sample success!")
    else:
        print("sample failure!")

    time.sleep(1)
    result = process("in")
    print(result)
