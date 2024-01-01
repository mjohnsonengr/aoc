import sys


def main(input):
  (timeLine, distanceLine) = input.split("\n")
  time = int(''.join(timeLine.split()[1:]))
  distance = int(''.join(distanceLine.split()[1:]))
  return howManyWaysToWin(time, distance)


def howManyWaysToWin(time, targetDistance):
  # speed = timeHeld
  # timeMoving = time - timeHeld
  # distance = timeHeld * timeMoving
  # find the smallest timeHeld and largest timeHeld where distance >= targetDistance
  # return largest - smallest
  # smallest:
  distances = [timeHeld for timeHeld in range(
      time) if timeHeld * (time - timeHeld) > targetDistance]
  return len(distances)


if __name__ == "__main__":
  if len(sys.argv) != 2:
    print("Usage: python part1.py <input_file>")
    sys.exit(1)

  input_file = sys.argv[1]
  with open(input_file) as f:
    print(main(f.read()))
