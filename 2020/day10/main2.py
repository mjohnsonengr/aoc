
def process(file):
    with open(file) as f:
        l = [int(i) for i in f]
    l.sort()
    # if (len(l) < 500):
    #     print(l)

    memo = dict()

    return combo(l, 0, memo)

def combo(l, i, memo):
    if i in memo:
        return memo[i]
    if i == len(l)-1:
        # print(s + [l[i]])
        return 1
    count = 0
    for j in range(i+1, min(i+4, len(l))):
        if l[j] - 3 <= l[i]:
            count += combo(l, j, memo)
    memo[i] = count
    return count


# result = process("sample.txt")
result = process("input.txt")
# result = process("sample2.txt")
# result = process("sample3.txt")
print(result)
