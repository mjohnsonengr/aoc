import re

l = []
with open('input.txt') as f:
    current = []
    for line in f:            
        if line.strip() == "":                
            values = {}
            for item in current:
                kv = item.split(":")
                values[kv[0]] = kv[1]
            l.append(values)
            current = []
        else:
            current += line.split()

values = {}
for item in current:
    kv = item.split(":")
    values[kv[0]] = kv[1]
l.append(values)
current = []

n = len(l)
# m = len(l[0])

fields = {
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
    "cid"
}
fields2 = {
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
}


def isValid(line):
    if not fields2.issubset(set(line.keys())):
        return False
    if not isFourDigits(line["byr"], 1920, 2002):
        # print(f'{line["byr"]} byr')
        return False
    if not isFourDigits(line["iyr"], 2010, 2020):
        # print(f'{line["iyr"]} iyr')
        return False
    if not isFourDigits(line["eyr"], 2020, 2030):
        # print(f'{line["eyr"]} eyr')
        return False
    if not isCmOrIn(line["hgt"]):
        # print(f'{line["hgt"]} hgt')
        return False
    if not isValidHcl(line["hcl"]):
        # print(f'{line["hcl"]} hcl')
        return False
    if not isValidEcl(line["ecl"]):
        # print(f'{line["ecl"]} ecl')
        return False
    if not isValidPid(line["pid"]):
        print(f'{line["pid"]} pid')
        return False
    return True
    
def isFourDigits(i, min, max):
    match = re.match(r"[0-9]{4}", i)
    if match:
        num = int(match.group(0))
        if num >= min and num <= max:
            return True
    return False

def isCmOrIn(s):
    cm = re.match(r"([0-9]{3})cm", s)
    if cm:
        num = int(cm.group(1))
        return num >= 150 and num <= 193
    inch = re.match(r"([0-9]{2})in", s)
    if inch:
        num = int(inch.group(1))
        return num >= 59 and num <= 76
    return False

def isValidHcl(s):
    match = re.match(r"#[0-9a-f]{6}", s)
    if match:
        return True
    return False

validEcl = [
    "amb",
    "blu",
    "brn",
    "gry",
    "grn",
    "hzl",
    "oth"
]
def isValidEcl(s):
    return s in validEcl

def isValidPid(s):
    print(s)
    match = re.match(r"^[0-9]{9}$", s)
    if match:
        return True
    return False

count = 0
for line in l:
    if isValid(line):
        count += 1
print(count)
