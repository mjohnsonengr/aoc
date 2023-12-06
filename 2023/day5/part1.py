import sys


def main(input):
    sections = input.split("\n\n")
    seeds = [int(x) for x in sections[0].split(": ")[1].split()]
    raw_maps = [m.splitlines()[1:] for m in sections[1:]]
    maps = [parseMap(m) for m in raw_maps]
    print(maps)
    return min([processSeed(seed, maps) for seed in seeds])


def parseMap(m):
    # m is a list of lines in a particular map
    # each line is: `dest source length`
    # return a list of ranges sorted by source
    return Map([tuple(map(int, l.split())) for l in m])


def processSeed(seed, maps):
    vals = [seed]
    curVal = seed
    for map in maps:
        curVal = map.translate(curVal)
        vals.append(curVal)
    print(vals)
    return curVal


class Map:
    # a range is a three-tuple of (source, dest, length)
    def __init__(self, ranges):
        self.ranges = sorted(ranges, key=lambda r: r[1])

    def translate(self, source):
        # given an int source, find the range with the largest source that's less than or equal to given source
        for r in reversed(self.ranges):
            (dest, src, length) = r
            if src <= source:
                # e.g. source=51, and ranges are (50, 52, 48) and (98, 50, 2)
                # picked r is (50, 52, 48)
                # return 52 + (51 - 50) = 53
                if source <= src + length:
                    translated = dest + (source - src)
                    print(f"{source} -> {translated}")
                    return translated
                print(f"{source} -> {source}")
                return source
        # Happens when source is smaller than all ranges
        return source

    def __repr__(self):
        return f"Map({self.ranges})"


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python part1.py <input_file>")
        sys.exit(1)

    input_file = sys.argv[1]
    with open(input_file) as f:
        print(main(f.read()))
