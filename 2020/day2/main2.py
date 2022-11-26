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
    r = [True for l in f.readlines() if valid(l)]

print(len(r))