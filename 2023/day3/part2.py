import sys
import re


def process(lines):
    gears = {}  # x, y (c, r)
    for i, line in enumerate(lines):
        if len(line) == 0:
            continue
        curnum = ""
        digits = 0
        for j, c in enumerate(line + "."):
            if c.isdigit():
                digits += 1
                curnum += c
            else:
                if len(curnum) == 0:
                    continue
                num = int(curnum)
                # for 2 digit number at pos 3, check [2,3,4,5]
                # in lines i-1, i, i+1
                # j is currently 1 after end of number

                def check_line(r, line):
                    for k in range(max(0, j - digits - 1), min(len(line), j + 1)):
                        c = line[k]
                        if c == "*":
                            if gears.get((k, r)) is not None:
                                gears[(k, r)][1] = num
                            else:
                                gears[(k, r)] = [num, 0]

                if i > 0:
                    check_line(i - 1, lines[i - 1])
                check_line(i, lines[i])
                if i < len(lines) - 1:
                    check_line(i + 1, lines[i + 1])
                curnum = ""
                digits = 0

    print(gears)
    total_sum = 0
    for gear in gears.values():
        total_sum += gear[0] * gear[1]

    return total_sum


def find_numbers(string):
    numbers = re.findall(r"\d+", string)
    return numbers


def main():
    if len(sys.argv) != 2:
        print("Usage: python part1.py <input_file>")
        return

    input_file = sys.argv[1]
    with open(input_file) as f:
        print(process(f.read().splitlines()))


main()
