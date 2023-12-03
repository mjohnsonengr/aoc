def processLine(line):
  if len(line) < 1: return 0
  """ Returns Game ID if game was possible with only 12 red, 13 green and 14 blue. 0 otherwise. """
  split = [[[d.split(' ') for d in c.split(', ')] for c in b.split('; ')] for b in line.split(': ')]
  try:
    gameId = int(split[0][0][0][1])
  except IndexError:
    print('Invalid line: ' + line)
    return 0

  games = split[1]
  for game in games:
    for piece in game:
      color = piece[1]
      # print("'" + color + "'")
      count = int(piece[0])
      if color not in ['red', 'green', 'blue']:
        # print('Invalid color: ' + color)
        return 0
      if color == 'red' and count > 12:
        # print('Too many red pieces: ' + str(count))
        return 0
      if color == 'green' and count > 13:
        # print('Too many green pieces: ' + str(count))
        return 0
      if color == 'blue' and count > 14:
        # print('Too many blue pieces: ' + str(count))
        return 0
  # print('Game ID: ' + str(gameId))
  return gameId


def main():
  with open('2023/day2/input.txt') as f:
      print(sum([processLine(l) for l in f.read().splitlines()]))

main()