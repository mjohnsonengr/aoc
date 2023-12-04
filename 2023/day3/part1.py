import sys
import re


def process(lines):
    sum = 0
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

                def check_line(line):
                    for c in line[max(0, j - digits - 1) : j + 1]:
                        if c.isdigit():
                            continue
                        if c == ".":
                            continue
                        return True
                    return False

                if (
                    (i > 0 and check_line(lines[i - 1]))
                    or check_line(lines[i])
                    or (i < len(lines) - 1 and check_line(lines[i + 1]))
                ):
                    print(f"{num} is a part number")
                    sum += num
                else:
                    print(f"{num} is NOT a part number")
                curnum = ""
                digits = 0
    return sum


def main():
    if len(sys.argv) != 2:
        print("Usage: python part1.py <input_file>")
        return

    input_file = sys.argv[1]
    with open(input_file) as f:
        print(process(f.read().splitlines()))


main()
