import sys


def main(lines):
    cards = [1] * len(lines)
    for line in lines:
        tryProcessLine(cards, line)
    return sum(cards)


def tryProcessLine(cards, line):
    try:
        processLine(cards, line)
    except Exception as e:
        print(f"Error processing line: {line}\n{e}")


def processLine(cards, line):
    if len(line) == 0:
        return

    [header, body] = line.split(":")
    [_, id] = header.split()
    [wCard, mCard] = body.split("|")
    wList = [int(n) for n in wCard.strip().split()]
    mList = [int(n) for n in mCard.strip().split()]
    winning = set(wList)
    nums = set(mList)
    card = int(id)

    numWin = len(winning.intersection(nums))
    for i in range(card, min(len(cards), card + numWin)):
        cards[i] += cards[card - 1]
    return


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python part1.py <input_file>")
        sys.exit(1)

    input_file = sys.argv[1]
    with open(input_file) as f:
        print(main([line for line in f.read().splitlines() if line]))
