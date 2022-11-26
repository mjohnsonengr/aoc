from collections import Counter

def process(file):
    sum = 0
    with open(file) as f:
        cur = []
        group = 0
        for l in f:
            if l.strip() is not "":
                cur += l.strip()
                group += 1
            else:
                s = "".join(cur)
                c = Counter(s)
                for k in c.keys():
                    if c[k] == group:
                        sum += 1
                cur = []
                group = 0

        s = "".join(cur)
        c = Counter(s)
        for k in c.keys():
            if c[k] == group:
                sum += 1

    return sum

# result = process("sample.txt")
result = process("input.txt")
print(result)