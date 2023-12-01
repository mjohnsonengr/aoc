def digit(s):
    if len(s) < 1: return None
    if s[0].isdigit(): return s[0]
    if s.startswith('one'): return '1'
    if s.startswith('two'): return '2'
    if s.startswith('three'): return '3'
    if s.startswith('four'): return '4'
    if s.startswith('five'): return '5'
    if s.startswith('six'): return '6'
    if s.startswith('seven'): return '7'
    if s.startswith('eight'): return '8'
    if s.startswith('nine'): return '9'

def isdigit(s):
    if digit(s) is not None: return True
    return False

def _numbers(s):
    first = None
    for i in range(len(s)):
        end = min(i+5, len(s)-1)
        a = digit(s[i:end])
        if a is not None:
            first = a
            break
    for i in range(len(s)-1, -1, -1):
        end = min(i+5, len(s)-1)
        a = digit(s[i:end])
        if a is not None:
            last = a
            break
    return int(first + last)
    
def numbers(s):
    a = _numbers(s)
    print(a)
    return a

#with open('day1/sample.txt') as f:
with open('day1/input.txt') as f:
    arr = [numbers(l) for l in f.readlines()]

print(sum(arr))
