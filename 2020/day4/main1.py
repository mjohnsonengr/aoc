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

count = 0
for line in l:
    # "cid" optional
    sorted = list(line.keys())
    sorted.sort()
    if fields2.issubset(set(line.keys())):
        print("O:" + str(sorted))
        count += 1
    else:
        print("X:" + str(sorted))




print(count)
