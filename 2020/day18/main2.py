from itertools import groupby
import numpy as np
import math
from pprint import PrettyPrinter
import re
import sys
import time


def process(file):
    sum = 0
    with open(file) as f:
        for line in f:
            sum += evaluate(line.rstrip())
    return sum


def evaluate(expr):
    # Looks like no double digit numbers but we'll see
    print()
    return _evaluate(list("".join(expr.split())))

def _evaluate(expr):
    # base case just in case
    if '+' not in expr and '*' not in expr and '(' not in expr:
        # ensure no double digits
        assert len(expr) == 1
        return int(expr[0])

    print(f"evaluate: {expr}")
    while '(' in expr:
        parens = []
        close = []
        for i, elem in enumerate(expr):
            if elem == '(':
                parens.append(i)
            if elem == ')':
                close.append(i)
                if len(parens) == len(close):
                    print(parens, close)
                    # do outermost; let recursion handle inner
                    start = parens.pop(0)
                    end = close.pop()
                    break
        print(f"handling paren {expr[start+1:end]}")
        val = _evaluate(expr[start+1:end])
        expr = expr[:start] + [str(val)] + expr[end+1:]
        print(f"handled paren; new: {expr}")

    if '*' in expr:
        return _evaluate(expr[:expr.index('*')]) * _evaluate(expr[expr.index('*')+1:])

    operand1 = int(expr[0])
    op = ""
    for elem in expr[1:]:
        if elem.isnumeric():
            assert op is not None
            if op == '+':
                operand1 = operand1 + int(elem)
            if op == '*':
                operand1 = operand1 * int(elem)
        if elem in ['+', '*']:
            op = elem
    print(operand1)
    return operand1


# https://stackoverflow.com/a/29525218/1405720
def rindex(mylist, myvalue):
    return len(mylist) - mylist[::-1].index(myvalue) - 1


if __name__ == "__main__":
    # TODO: Make this take input as a flag
    pp = PrettyPrinter()

    assert evaluate("1 + 2 * 3 + 4 * 5 + 6") == 231
    assert evaluate("2 * 3 + (4 * 5)") == 46
    assert evaluate("5 + (8 * 3 + 9 + 3 * 4 * 3)") == 1445
    assert evaluate("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") == 669060
    assert evaluate("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 23340

    time.sleep(1)
    result = process("in")
    print(result)
