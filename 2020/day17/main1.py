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
    pocket[0] = {y:{x:cube for x, cube in enumerate(line)} for y, line in enumerate(lines)}
    pp.pprint(pocket)

    minX = 0
    minY = 0
    minZ = 0
    maxX = len(lines[0]) - 1
    maxY = len(lines) - 1
    maxZ = 0

    for i in range(6):
        new_pocket = {}
        # grow the pocket dimension
        minZ -= 1
        minY -= 1
        minX -= 1
        maxZ += 1
        maxY += 1
        maxX += 1
        for z in range(minZ, maxZ + 1):
            for y in range(minY, maxY + 1):
                for x in range(minX, maxX + 1):

                    # Count active neighbors
                    active = []
                    neighbors = []
                    for z1 in range(z-1, z+2):
                        for y1 in range(y-1, y+2):
                            for x1 in range(x-1, x+2):
                                if x == x1 and y == y1 and z == z1:
                                    # don't count self
                                    continue
                                neighbors.append((x1, y1, z1))
                                neighbor = get_cube(pocket, x1, y1, z1)
                                if neighbor == '#':
                                    active.append((x1, y1, z1))
                                else:
                                    assert neighbor == '.'
                    assert len(neighbors) == 26
                    if get_cube(pocket, x, y, z) == '#':
                        set_cube(new_pocket, x, y, z, '#' if len(active) in [2,3] else '.')
                    else:
                        set_cube(new_pocket, x, y, z, '#' if len(active) == 3 else '.')
        pocket = new_pocket
        print_pocket(pocket, minX, minY, minZ, maxX, maxY, maxZ)


    result = count_active(pocket)
    print(result)
    return result


def get_cube(pocket, x, y, z):
    try:
        return pocket[z][y][x]
    except KeyError:
        return '.'


def set_cube(pocket, x, y, z, val):
    layer = pocket.setdefault(z, {})
    line = layer.setdefault(y, {})
    line[x] = val


def print_pocket(pocket, minX, minY, minZ, maxX, maxY, maxZ):
    for z in range(minZ, maxZ + 1):
        print(f"z = {z}")
        for y in range(minY, maxY + 1):
            line = ""
            for x in range(minX, maxX + 1):
                line += get_cube(pocket, x, y, z)
            print(line)

        print()



def count_active(pocket):
    return sum([sum([list(line.values()).count('#') for line in layer.values()]) for layer in pocket.values()])


if __name__ == "__main__":
    # TODO: Make this take input as a flag
    pp = PrettyPrinter()

    assert process("smp") == 112

    time.sleep(1)
    result = process("in")
    print(result)
