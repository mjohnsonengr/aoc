def processLine(line):
  if len(line) < 1: return 0
  """ Returns Game ID if game was possible with only 12 red, 13 green and 14 blue. 0 otherwise. """
  split = [[[d.split(' ') for d in c.split(', ')] for c in b.split('; ')] for b in line.split(': ')]
  try:
    gameId = int(split[0][0][0][1])
  except IndexError:
    print('Invalid line: ' + line)
    return 0

  red = 0
  blue = 0
  green = 0

  games = split[1]
  for game in games:
    for piece in game:
      color = piece[1]
      count = int(piece[0])
      if color == 'red': red = max(red, count)
      if color == 'green': green = max(green, count)
      if color == 'blue': blue = max(blue, count)
  return red*green*blue


def main():
  with open('2023/day2/input.txt') as f:
      print(sum([processLine(l) for l in f.read().splitlines()]))

main()