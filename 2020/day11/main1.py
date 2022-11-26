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
    newSeats = []
    for i in range(0, len(seats)):
        newRow = []
        for j in range(0, len(seats[0])):
            minRow = max(0, i-1)
            maxRow = min(i+1, len(seats)-1)
            minCol = max(0, j-1)
            maxCol = min(j+1, len(seats[0])-1)
            # check 8 positions
            adj = 0
            j2 = minCol
            i2j2 = []
            for i2 in range(minRow, maxRow+1):
                for j2 in range(minCol, maxCol+1):
                    i2j2.append((i2, j2))
            for i2, j2 in i2j2:
                if i == i2 and j == j2:
                    continue
                if seats[i2][j2] == '#':
                    adj += 1


            if seats[i][j] == 'L':
                newRow.append('#' if adj == 0 else 'L')
            elif seats[i][j] == '#':
                newRow.append('L' if adj >= 4 else '#')
            elif seats[i][j] == '.':
                newRow.append('.')

        newSeats.append(''.join(newRow))
    return newSeats

# result = process("sample.txt")
result = process("input.txt")
print(result)
