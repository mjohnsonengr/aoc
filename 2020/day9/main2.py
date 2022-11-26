import re
from collections import deque

def process(inv, file):
    with open(file) as f:
        nums = [int(i) for i in f]
        for i in range(0, len(nums)):
            sum = 0
            j = i
            while sum < inv:
                print((j, sum))
                sum += nums[j]
                j += 1
            print(f"sum: {sum}")
            if sum == inv:
                sub = nums[i:j]
                return min(sub)+max(sub)

            

        
def xmas(pre, nums):
    for i in range(pre, len(nums)):
        num = nums[i]
        print(f"num: {num}")
        able = set(nums[i-pre:i])

        for j in range(i-pre, i):
            print(f"inner: {nums[j]}")
            print(f"search: {num - nums[j]}")
            if int(num - nums[j]) in able:
                break
        else:
            return num
    print("wtf")



# result = process(127, "sample.txt")
# result = process("sample2.txt")
result = process(756008079, "input.txt")
print(result)
