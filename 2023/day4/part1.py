import sys


def main(lines):
    return sum([tryProcessLine(line) for line in lines])


def tryProcessLine(line):
    try:
        return processLine(line)
    except Exception as e:
        print(f"Error processing line: {line}\n{e}")


def processLine(line):
    [header, body] = line.split(":")
    [_, id] = header.split()
    [wCard, mCard] = body.split("|")
    wList = [int(n) for n in wCard.strip().split()]
    mList = [int(n) for n in mCard.strip().split()]
    winning = set(wList)
    nums = set(mList)

    numWin = len(winning.intersection(nums))
    if numWin == 0:
        return 0
    return 2 ** (len(winning.intersection(nums)) - 1)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python part1.py <input_file>")
        sys.exit(1)

    input_file = sys.argv[1]
    with open(input_file) as f:
        print(main([line for line in f.read().splitlines() if line]))
