import re
from collections import deque

def process(file):
    program = []
    with open(file) as f:
        for l in f:
            m = re.match(r"([a-z]+) ([+\-0-9]+)", l)
            instr = m.group(1)
            val = int(m.group(2))
            program.append((instr, val))

    return runProgram(program)
        
def runProgram(program):
    visited = set()
    ptr = 0
    acc = 0
    while True:
        instr, val = program[ptr]

        if ptr in visited:
            return acc

        ptr, acc = ops[instr](ptr, acc, val)
        visited.add(ptr)
        ptr += 1

def acc(ptr, acc, val):
    return (ptr, acc + val)

def jmp(ptr, acc, val):
    # don't jump too far
    return (ptr + val - 1, acc)

def nop(ptr, acc, val):
    return (ptr, acc)

ops = {
    "acc": acc,
    "jmp": jmp,
    "nop": nop
}

# result = process("sample.txt")
# result = process("sample2.txt")
result = process("input.txt")
print(result)