from itertools import groupby
import numpy as np
import math
from pprint import PrettyPrinter
import re
import sys
import time


def process(file):
    with open(file) as f:
        lines = [line.rstrip() for line in f]

    pocket = {}
    pocket[0] = {0: {y: {x: cube for x, cube in enumerate(
        line)} for y, line in enumerate(lines)}}
    pp.pprint(pocket)

    minX = 0
    minY = 0
    minZ = 0
    minW = 0
    maxX = len(lines[0]) - 1
    maxY = len(lines) - 1
    maxZ = 0
    maxW = 0

    for i in range(6):
        new_pocket = {}
        # grow the pocket dimension
        minW -= 1
        minZ -= 1
        minY -= 1
        minX -= 1
        maxW += 1
        maxZ += 1
        maxY += 1
        maxX += 1
        for w in range(minW, maxW + 1):
            for z in range(minZ, maxZ + 1):
                for y in range(minY, maxY + 1):
                    for x in range(minX, maxX + 1):

                        # Count active neighbors
                        active = []
                        neighbors = []
                        for w1 in range(w-1, w+2):
                            for z1 in range(z-1, z+2):
                                for y1 in range(y-1, y+2):
                                    for x1 in range(x-1, x+2):
                                        if x == x1 and y == y1 and z == z1 and w == w1:
                                            # don't count self
                                            continue
                                        neighbors.append((x1, y1, z1, w1))
                                        neighbor = get_cube(
                                            pocket, x1, y1, z1, w1)
                                        if neighbor == '#':
                                            active.append((x1, y1, z1, w1))
                                        else:
                                            assert neighbor == '.'
                        assert len(neighbors) == 80
                        if get_cube(pocket, x, y, z, w) == '#':
                            set_cube(new_pocket, x, y, z, w, '#' if len(
                                active) in [2, 3] else '.')
                        else:
                            set_cube(new_pocket, x, y, z, w,
                                     '#' if len(active) == 3 else '.')
        pocket = new_pocket
        # print_pocket(pocket, minX, minY, minZ, minW, maxX, maxY, maxZ, maxW)

    result = count_active(pocket)
    print(result)
    return result


def get_cube(pocket, x, y, z, w):
    try:
        return pocket[w][z][y][x]
    except KeyError:
        return '.'


def set_cube(pocket, x, y, z, w, val):
    hypercube = pocket.setdefault(w, {})
    layer = hypercube.setdefault(z, {})
    line = layer.setdefault(y, {})
    line[x] = val


def print_pocket(pocket, minX, minY, minZ, minW, maxX, maxY, maxZ, maxW):
    for w in range(minW, maxW + 1):
        print("="*15)
        print(f"w = {w}")
        for z in range(minZ, maxZ + 1):
            print(f"z = {z}")
            for y in range(minY, maxY + 1):
                line = ""
                for x in range(minX, maxX + 1):
                    line += get_cube(pocket, x, y, z, w)
                print(line)
            print()
        print()
        print()


def count_active(pocket):
    return sum([sum([sum([list(line.values()).count('#') for line in layer.values()]) for layer in hypercube.values()]) for hypercube in pocket.values()])


    # TODO: Make this take input as a flag
    pp = PrettyPrinter()


    assert process("smp") == 848    assert evaluate("1 + 2 * 3 + 4 * 5 + 6") == 71

    result = process("in")
    print(result)
