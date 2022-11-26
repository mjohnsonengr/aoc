import numpy as np
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
    wx = 10.0
    wy = 1.0
    xpos = 0.0
    ypos = 0.0

    for cmd in cmds:
        xpos, ypos, wx, wy = instr[cmd[0]](xpos, ypos, wx, wy, cmd[1])
        print(cmd)
        print((xpos, ypos, wx, wy))

    return abs(xpos) + abs(ypos)

def north(xpos, ypos, wx, wy, val):
    return (xpos, ypos, wx, wy + val)

def south(xpos, ypos, wx, wy, val):
    return (xpos, ypos, wx, wy - val)

def east(xpos, ypos, wx, wy, val):
    return (xpos, ypos, wx + val, wy)

def west(xpos, ypos, wx, wy, val):
    return (xpos, ypos, wx - val, wy)

def forward(xpos, ypos, wx, wy, val):
    return (xpos + wx*val, ypos + wy*val, wx, wy)

def right(xpos, ypos, wx, wy, val):
    r, t = cart2pol(wx, wy)
    tnew = math.radians(math.degrees(t)-val)
    print(t, math.degrees(t), math.degrees(t)-val, tnew)
    wx, wy = pol2cart(r, tnew)
    return xpos, ypos, wx, wy

def left(xpos, ypos, wx, wy, val):
    r, t = cart2pol(wx, wy)
    tnew = math.radians(math.degrees(t)+val)
    print(t, math.degrees(t), math.degrees(t)+val, tnew)
    wx, wy = pol2cart(r, tnew)
    return xpos, ypos, wx, wy

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

# https://stackoverflow.com/a/26757297/1405720
def cart2pol(x, y):
    rho = np.sqrt(x**2 + y**2)
    phi = np.arctan2(y, x)
    return(rho, phi)

def pol2cart(rho, phi):
    x = rho * np.cos(phi)
    y = rho * np.sin(phi)
    return(x, y)

instr = {
    'N': north,
    'S': south,
    'W': west,
    'E': east,
    'F': forward,
    'R': right,
    'L': left
}

# result = process("sample.txt")
result = process("input.txt")
print(result)
