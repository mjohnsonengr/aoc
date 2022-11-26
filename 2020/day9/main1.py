import re
from collections import deque

def process(pre, file):
    with open(file) as f:
        nums = [int(i) for i in f]
        return xmas(pre, nums)

        
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



result = process(5, "sample.txt")
print(result)
# result = process("sample2.txt")
result = process(25, "input.txt")
print(result)
