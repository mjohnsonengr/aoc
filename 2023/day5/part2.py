import sys
from itertools import chain
import os


def main(input):
    # Part 2 idea:
    # 1. take the maps in reverse order
    # 2. find the smallest destination in last map; if it doesn't start with 0, get the set of 0 to either the smallest
    # 3. find the minimal set of mapped ranges in the 2nd to last map with destinations in that range's source
    # 4. repeat 3 until you get to the first map
    # 5. if no seeds match,

    """
    Example
    Smallest location: (0 0 56)
    Smallest humitity mapping to that: (0 69 1)
    -- since len is 1 now, it's just simple translating dest->src
    temp: 65
    light: 58
    water: 108
    fertilizer: 108
    soil: 108
    seed: 108

    That didn't work so try again with next smallest humidity: (1 0 69) -> (1 0 55) {src [1-56]}
    light-to-temp: (0 0 45) {src [1-45]}
    water-to-light: (0 0 18) {src [1-18]}
    fertilizer-to-water: (0 11 42) {src [12-29]} -- src range incremented by (src-dest)
    soil-to-fertilizer: (0 15 37) {src [27-44]}
    seed-to-soil: (0 0 50) {src [27-44]}

    That didn't work, so try again with next smallest at each stage
    This may not be super efficient.

    Another idea from brief read of Reddit: keep track of seed ranges, merely splitting them
    """

    sections = input.split("\n\n")
    raw_seeds = [int(x) for x in sections[0].split(": ")[1].split()]
    seeds = [(raw_seeds[i], raw_seeds[i + 1]) for i in range(0, len(raw_seeds), 2)]
    raw_maps = [m.splitlines()[1:] for m in sections[1:]]
    maps = [parseMap(m) for m in raw_maps]
    print(maps)
    return min(list(chain(*[processSeedRange(seedRange, maps) for seedRange in seeds])))


def parseMap(m):
    # m is a list of lines in a particular map
    # each line is: `dest source length`
    # return a list of ranges sorted by source
    return Map([tuple(map(int, l.split())) for l in m])


def processSeedRange(seedRange, maps):
    curRanges = [seedRange]
    for map in maps:
        curRanges = map.translateRanges(curRanges)
        print(curRanges)
    return curRanges


class Map:
    # a range is a three-tuple of (source, dest, length)
    def __init__(self, ranges):
        # sort by source
        self.ranges = sorted(ranges, key=lambda r: r[1])

    def translateRanges(self, curRanges):
        # given a list of ranges, return new ranges from this map
        return list(chain(*[self.translateRange(curRange) for curRange in curRanges]))

    def translateRange(self, curRange):
        # case 1 (48, 4) and [(50, 98, 2), (52, 50, 48)]] -> [(48, 2), (52, 2)]
        (start, count) = curRange
        end = start + count
        i = start
        newRanges = []
        while i < end:
            (dest, src, length) = self.fitRange(i, self.getRange(i))
            if i + length > end:
                length = end - i
            i += length
            newRanges.append((dest, length))
        return newRanges

    def getRange(self, val):
        # given an int val, find the range with the largest src that's less than or equal to given val
        for i, r in reversed(list(enumerate(self.ranges))):
            (dest, src, length) = r
            if src <= val:
                # e.g. val=51, and ranges are (52, 50, 48) and (50, 98, 2)
                # picked r is (52, 50, 48)
                if val < src + length:
                    return r
                if i < len(self.ranges) - 1:
                    # new range that is between this and the next range
                    nextRange = self.ranges[i + 1]
                    nextStart = nextRange[1]
                    curEnd = src + length
                    return (curEnd, curEnd, nextStart - curEnd)
                else:
                    # Happens when val is larger than all ranges
                    return (val, val, 2**63 - 1)
        # Happens when val is smaller than all ranges
        # return the initial range
        return (0, 0, self.ranges[0][1])

    def fitRange(self, val, range):
        # (50, 98, 2) given 99 should be (51, 99, 1)
        # (0, 0, 50) given 48 should be (48, 48, 2)
        (dest, src, length) = range
        diff = val - src
        return (dest + diff, src + diff, length - diff)

    def __repr__(self):
        return f"Map({self.ranges})"


if __name__ == "__main__":
    if len(sys.argv) != 2:
        input_file = f"{os.path.dirname(__file__)}/s"
    else:
        input_file = sys.argv[1]

    # This one was complicated; needed tests to keep logic straight
    testMap = Map([(50, 98, 2), (52, 50, 48)])
    assert testMap.getRange(48) == (0, 0, 50)
    assert testMap.getRange(50) == (52, 50, 48)

    assert testMap.fitRange(48, (0, 0, 50)) == (48, 48, 2)
    assert testMap.fitRange(99, (50, 98, 2)) == (51, 99, 1)

    assert testMap.translateRange((48, 4)) == [
        (48, 2),
        (52, 2),
    ]
    assert testMap.translateRange((96, 4)) == [
        (98, 2),
        (50, 2),
    ]

    with open(input_file) as f:
        print(main(f.read()))
