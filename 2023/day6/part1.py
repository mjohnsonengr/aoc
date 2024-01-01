import sys


def main(input):
  (timeLine, distanceLine) = input.split("\n")
  times = [int(x) for x in timeLine.split()[1:]]
  distances = [int(x) for x in distanceLine.split()[1:]]
  result = 1
  return [
      result := result * x
      for x in [howManyWaysToWin(t, d) for (t, d) in zip(times, distances)]
  ][-1]


def howManyWaysToWin(time, targetDistance):
  # speed = timeHeld
  # timeMoving = time - timeHeld
  # distance = timeHeld * timeMoving
  # find the smallest timeHeld and largest timeHeld where distance >= targetDistance
  # return largest - smallest
  # smallest:
  distances = [timeHeld for timeHeld in range(
      time) if timeHeld * (time - timeHeld) > targetDistance]
  print(distances)
  return len(distances)


if __name__ == "__main__":
  if len(sys.argv) != 2:
    print("Usage: python part1.py <input_file>")
    sys.exit(1)

  input_file = sys.argv[1]
  with open(input_file) as f:
    print(main(f.read()))
