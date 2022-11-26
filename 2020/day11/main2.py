import pprint
import sys

def process(file):
    pp = pprint.PrettyPrinter()
    with open(file) as f:
        seats = [l.strip() for l in f]
    pp.pprint(seats)

    print("======= first iteration =====")
    newSeats = applyRules(seats)
    pp.pprint(newSeats)
    maxCount = sys.maxsize
    # maxCount = 2
    count = 1
    while seats != newSeats and count < maxCount:
        seats = newSeats
        newSeats = applyRules(seats)
        count += 1
        print()
        print(f"======== iteration {count} ========")
        # pp.pprint(newSeats)
    seats = newSeats
    pp.pprint(seats)
    return sum([sum([1 for i in row if i == '#']) for row in seats])

def applyRules(seats):
    rowLen = len(seats)
    colLen = len(seats[0])

    def valid(i2, j2):
        return i2 < rowLen and j2 < colLen and i2 >= 0 and j2 >= 0

    newSeats = []
    for i in range(0, rowLen):
        newRow = []
        for j in range(0, colLen):
            minRow = max(0, i-1)
            maxRow = min(i+1, rowLen-1)
            minCol = max(0, j-1)
            maxCol = min(j+1, colLen-1)
            # check 8 positions
            adj = 0
            j2 = minCol
            i2j2 = []
            # print(f"seat: ({i}, {j})")
            for r in [-1, 0, +1]:
                for c in [-1, 0, +1]:
                    if r == 0 and c == 0:
                        continue
                    i2 = i + r
                    j2 = j + c
                    while valid(i2, j2) and seats[i2][j2] == '.':
                        i2 += r
                        j2 += c
                    if valid(i2, j2):
                        i2j2.append((i2, j2))
            # print(i2j2)
            for i2, j2 in i2j2:
                if i == i2 and j == j2:
                    continue
                if seats[i2][j2] == '#':
                    adj += 1


            if seats[i][j] == 'L':
                newRow.append('#' if adj == 0 else 'L')
            elif seats[i][j] == '#':
                newRow.append('L' if adj >= 5 else '#')
            elif seats[i][j] == '.':
                newRow.append('.')

        newSeats.append(''.join(newRow))
    return newSeats

# result = process("sample.txt")
result = process("input.txt")
print(result)
