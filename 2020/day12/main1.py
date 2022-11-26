import math
import pprint
import sys

def process(file):
    pp = pprint.PrettyPrinter()
    with open(file) as f:
        l = [cmd(l.strip()) for l in f]

    pp.pprint(l)
    return run(l)

def cmd(s):
    return (s[0], int(s[1:]))

def run(cmds):
    # N = 90; S = 270; W = 180; E = 0
    # N/S = +/- y
    # E/W = +/- x
    xpos = 0.0
    ypos = 0.0
    dir = 0

    for cmd in cmds:
        xpos, ypos, dir = instr[cmd[0]](xpos, ypos, dir, cmd[1])

    return abs(xpos) + abs(ypos)

def north(xpos, ypos, dir, val):
    return (xpos, ypos + val, dir)

def south(xpos, ypos, dir, val):
    return (xpos, ypos - val, dir)

def east(xpos, ypos, dir, val):
    return (xpos + val, ypos, dir)

def west(xpos, ypos, dir, val):
    return (xpos - val, ypos, dir)

def forward(xpos, ypos, dir, val):
    return (xpos + val*cos(dir), ypos + val*sin(dir), dir)

def right(xpos, ypos, dir, val):
    return (xpos, ypos, deg(dir - val))

def left(xpos, ypos, dir, val):
    return (xpos, ypos, deg(dir + val))

def deg(num):
    while num > 360:
        num -= 360
    while num < 0:
        num += 360
    return num

def sin(val):
    return math.sin(math.radians(val))

def cos(val):
    return math.cos(math.radians(val))

instr = {
    'N': north,
    'S': south,
    'W': west,
    'E': east,
    'F': forward,
    'R': right,
    'L': left
}

result = process("sample.txt")
# result = process("input.txt")
print(result)
