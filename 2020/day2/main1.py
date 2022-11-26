import re

def getGroups(line):
    m = re.search(r'([0-9]+)-([0-9]+) (.): (.*)$', line)
    return tuple([m.group(i) for i in range(1,5)])

def valid(line):
    (low, high, ch, word) = getGroups(line)
    low = int(low)
    high = int(high)
    c = word.count(ch)
    return c >= low and c <= high
    
with open('input.txt') as f:
    r = [True for l in f.readlines() if valid(l)]

print(len(r))