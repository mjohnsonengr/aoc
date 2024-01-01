import sys
from functools import cmp_to_key


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
  bc = classifyHand(handB)
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
  # Sort the hand
  hand = sorted(hand)

  # Check for 5 of a kind
  if (hand[0] == hand[1] == hand[2] == hand[3] == hand[4]):
    return 7

  # Check for 4 of a kind
  if (hand[0] == hand[1] == hand[2] == hand[3]) or (hand[1] == hand[2] == hand[3] == hand[4]):
    return 6

  # Check for full house
  if (hand[0] == hand[1] == hand[2]) and (hand[3] == hand[4]):
    return 5
  if (hand[0] == hand[1]) and (hand[2] == hand[3] == hand[4]):
    return 5

  # Check for 3 of a kind
  if (hand[0] == hand[1] == hand[2]) or (hand[1] == hand[2] == hand[3]) or (hand[2] == hand[3] == hand[4]):
    return 4

  # Check for 2 pair
  if (hand[0] == hand[1]) and (hand[2] == hand[3]):
    return 3
  if (hand[0] == hand[1]) and (hand[3] == hand[4]):
    return 3
  if (hand[1] == hand[2]) and (hand[3] == hand[4]):
    return 3

  # Check for 1 pair
  if (hand[0] == hand[1]) or (hand[1] == hand[2]) or (hand[2] == hand[3]) or (hand[3] == hand[4]):
    return 2

  # High card
  return 1


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
    return 11
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
    sys.exit(1)

  input_file = sys.argv[1]
  with open(input_file) as f:
    print(main(f.read()))
