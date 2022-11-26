import re
from collections import deque

def process(file):
    with open(file) as f:
        # list of which bags cancontain key
        bags = {}
        # containers with a list of what they can contain
        containers = {}
        for l in f:
            match = re.match(r"([a-z\s]+) bags contain (.*)\n?", l)
            container = match.group(1)
            contents = match.group(2)
            contents_dict = {}
            for item in contents.split(','):
                if ("no other bags" in item):
                    continue
                m2 = re.match(r"\s?([0-9]+) ([a-z\s]+) bags?\.?", item)
                n = int(m2.group(1))
                bag = m2.group(2)
                if bag not in bags.keys():
                    bags[bag] = []
                bags[bag].append(container)
                contents_dict[bag] = n
            containers[container] = contents_dict
        print("===containers:")
        print(containers)
        print("===bags:")
        print(bags)

    # expand containers (uncount shiny gold)
    return getSize(containers, "shiny gold") - 1
        

def getSize(containers, item):
    d = containers[item]
    count = 1
    for b in d:
        count += d[b] * getSize(containers, b)
    return count



# result = process("sample.txt")
# result = process("sample2.txt")
result = process("input.txt")
print(result)