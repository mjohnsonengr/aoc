import numpy as np
import math
import pprint
import sys

def process(file):
    pp = pprint.PrettyPrinter()
    with open(file) as f:
        time = int(f.readline().strip('\n'))
        # time = f.readline()
        # buses = f.readline().split(',')
        buses = [int(bus) for bus in f.readline().split(',') if bus != 'x']
    print(time)
    print(buses)
    next = {bus:(math.floor(time/bus)+1)*bus for bus in buses}
    print(next)
    next = {x:y for y,x in next.items()}
    print(next)
    i = time
    while True:
        if i in next:
            bus = next[i]
            depart = i - time
            return bus * depart
        i += 1
    return None

# result = process("sample.txt")
result = process("input.txt")
print(result)
