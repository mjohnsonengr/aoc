import re

def getGroups(line):
    m = re.search(r'([0-9]+)-([0-9]+) (.): (.*)$', line)
    return tuple([m.group(i) for i in range(1,5)])

def onlyone(cond1, cond2):
    return (cond1 or cond2) and ((cond1 and not cond2) or (not cond1 and cond2))

def valid(line):
    (low, high, ch, word) = getGroups(line)
    low = int(low)
    high = int(high)
    result = onlyone(word[low-1] == ch, word[high-1] == ch)
    print(low, high, ch, word, word[low-1], word[high-1], result)
    return result
    
with open('input.txt') as f:
    r = [list(line) for line in f.readlines()]

lineLen = len(r[0])

def cs(dx, dy):
    x = 0
    y = 0
    count = 0
    for i in range(len(r)):
        print(x, y)
        if r[y][x] == '#':
            count += 1
            # r[y][x] = "X"
        # else:
            # r[y][x] = "O"
        y += dy 
        x += dx
        if x >= lineLen - 1:
            x -= (lineLen - 1)
        if y >= len(r):
            return count
    return count

print(cs(1, 1)*cs(3, 1)*cs(5, 1)*cs(7,1)*cs(1,2))