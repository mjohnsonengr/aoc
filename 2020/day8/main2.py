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

    print(runProgram(program))

    # get all variants of program.
    for i in range(len(program)):
        instr, val = program[i]
        newProgram = program.copy()
        if instr == "acc":
            continue
        if instr == "jmp":
            newProgram[i] = ("nop", val)
        if instr == "nop":
            newProgram[i] = ("jmp", val)
        success, acc = runProgram(newProgram)
        if success:
            return acc

    return "Something's wrong"
        
def runProgram(program):
    visited = set()
    ptr = 0
    acc = 0
    while True:
        if ptr >= len(program):
            return (True, acc)
        instr, val = program[ptr]
        if ptr in visited:
            return (False, acc)

        visited.add(ptr)
        ptr, acc = ops[instr](ptr, acc, val)
        if instr != "jmp":
            ptr += 1

def acc(ptr, acc, val):
    return (ptr, acc + val)

def jmp(ptr, acc, val):
    return (ptr + val, acc)

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
