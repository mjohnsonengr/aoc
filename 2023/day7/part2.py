import sys
from functools import cmp_to_key
from itertools import groupby


def main(input):
  hands = [line.split() for line in input.split("\n")]
  ranked = rankHands(hands)
  print(ranked)
  result = 0
  for (i, (h, b)) in enumerate(ranked):
    print(i, h, b)
    print((i+1)*int(b))
    result += (i+1)*int(b)
    print(result)
  return result


def rankHands(hands):
  # Returns a list sorted by rank; worst (smallest) is rank 1
  return sorted(hands, key=cmp_to_key(compareHands))


def compareHands(a, b):
  """ Compares two hands, returns 1 if a > b, 0 if a == b, -1 if a < b """
  # 1 if a is a better hand; -1 if b is a better hand; they should never be equal
  handA = a[0]
  handB = b[0]
  ac = classifyHand(handA)
  print(f"Hand A: {handA} is {ac}")
  bc = classifyHand(handB)
  print(f"Hand B: {handB} is {bc}")
  if (ac > bc):
    return 1
  elif (ac < bc):
    return -1
  else:
    for (cardA, cardB) in zip(handA, handB):
      comp = compareCards(cardA, cardB)
      if (comp != 0):
        return comp

  # Should never get here
  assert False, f"Hands are equal: {a}, {b}"


FIVE_OF_A_KIND = 7
FOUR_OF_A_KIND = 6
FULL_HOUSE = 5
THREE_OF_A_KIND = 4
TWO_PAIR = 3
ONE_PAIR = 2
HIGH_CARD = 1


def classifyHand(hand):
  """ Classifies a hand, returning an integer to represent; lowest int is worst hand

  5oak = 7
  4oak = 6
  full house = 5
  3oak = 4
  2 pair = 3
  1 pair = 2
  high card = 1

  Credit for the implementation of this function is nearly entirely due to Copilot.
  """
  # Get hand groupings: (card, count)
  groups = [(a, len(list(b)))
            for (a, b) in groupby(sorted(hand))]

  # 5 of a kind
  if len(groups) == 1:
    return FIVE_OF_A_KIND

  if len(groups) == 2:
    # If one is J, then it's a 5 of a kind.
    if hasJ(groups):
      return FIVE_OF_A_KIND

    # Otherwise there could be 4 of a kind or a full house
    if hasCount(4, groups):
      return FOUR_OF_A_KIND
    else:  # must be a 3,2 or 2,3
      return FULL_HOUSE

  if len(groups) == 3:
    # 3,1,1 or 2,2,1
    if hasJ(groups):
      # When there's a J, and one card has 3, that makes a 4 of a kind.
      if hasCount(3, groups) or getJ(groups) == 2:
        # other two groups have 1 each; no matter, it's a 4 of a kind
        return FOUR_OF_A_KIND
      else:
        # J is either 1 or 2. The other two groups are either (2,1) or (2,2))
        # In all of those cases, it's a full house
        return FULL_HOUSE

    # No J, so it's either a three of a kind or two pair
    return THREE_OF_A_KIND if hasCount(3, groups) else TWO_PAIR

  if len(groups) == 4:
    # 2,1,1,1
    # If any is a J, then it's 3 of a kind; else 2 of a kind
    return THREE_OF_A_KIND if hasJ(groups) else ONE_PAIR

  if len(groups) == 5:
    # 1,1,1,1,1
    return ONE_PAIR if hasJ(groups) else HIGH_CARD

  # Should never get here
  assert False, f"Unknown hand: {hand}"


def hasJ(groups):
  return "J" in [g[0] for g in groups]


def hasCount(count, groups):
  return count in [g[1] for g in groups]


def getJ(groups):
  assert hasJ(groups)
  return [g for g in groups if g[0] == "J"][0][1]


def compareCards(a, b):
  """ Compares two cards, returns 1 if a > b, 0 if a == b, -1 if a < b 

  Cards are chars, including J, Q, K and A.
  """
  ap = parseCard(a)
  bp = parseCard(b)
  if ap > bp:
    return 1
  elif ap < bp:
    return -1
  else:
    return 0


def parseCard(card):
  """ Returns an integer value for card, 2-14 (2-9, T, J, Q, K, A) """
  if card == "T":
    return 10
  elif card == "J":
    # Now worth even less than 2 in ordering
    return 1
  elif card == "Q":
    return 12
  elif card == "K":
    return 13
  elif card == "A":
    return 14
  else:
    return int(card)


if __name__ == "__main__":
  if len(sys.argv) != 2:
    print("Usage: python part1.py <input_file>")
    print("Using s")
    input_file = "2023/day7/s"
  else:
    input_file = sys.argv[1]

  with open(input_file) as f:
    print(main(f.read()))
