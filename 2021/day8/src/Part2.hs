module Part2 where

import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Sets

part2 :: [String] -> Int
part2 lns = sum $ map part2' lns

part2' :: String -> Int
part2' ln =
  let [key, code] = splitOn ["|"] . map sort $ splitOn " " ln
      keys = parseKey key
  in strToInt . intercalate "" $ map (maybe "" show . flip elemIndex keys) $ map sort code

strToInt :: String -> Int 
strToInt = read

-- returns [zero, one,..] where each is the sorted list of segments for that index's representation
parseKey :: [String] -> [String]
parseKey key =
  let
    one = head $ filter (\num -> length num == 2) key
    four = head $ filter (\num -> length num == 4) key
    seven = head $ filter (\num -> length num == 3) key
    eight = head $ filter (\num -> length num == 7) key
    a = difference seven four
    b = findChar key 6 $ Sets.fromList [a]
    c = findChar key 8 $ Sets.fromList [a,b]
    d = difference four $ b:one
    e = findChar key 4 $ Sets.fromList [a,b,c,d]
    f = findChar key 9 $ Sets.fromList [a,b,c,d,e]
    g = difference eight $ e:seven++four
    two = sort [a,c,d,e,g]
    three = sort [a,c,d,f,g]
    five = sort [a,b,d,f,g]
    six = sort [a,b,d,e,f,g]
    nine = sort [a,b,c,d,f,g]
    zero = sort [a,b,c,e,f,g]
  in [zero, one, two, three, four, five, six, seven, eight, nine]

difference :: String -> String -> Char
difference a b = head . Sets.toList $ Sets.difference (Sets.fromList a) (Sets.fromList b)

findChar :: [String] -> Int -> Set Char -> Char
findChar key n found = fst . head . filter (\(letter, count) -> count == n && not (Sets.member letter found)) $ segmentFreq key

segmentFreq :: [String] -> [(Char, Int)]
segmentFreq key = map (\x -> (head x, length x)) . group . sort $ intercalate "" key

{-
0: abcefg  -- 6
1: cf      -- 2 -- unique
2: acdeg   -- 5
3: acdfg   -- 5
4: bcdf    -- 4 -- unique
5: abdfg   -- 5
6: abdefg  -- 6
7: acf     -- 3 -- unique
8: abcdefg -- 7 -- unique
9: abcdfg  -- 6

how many lit: which numbers
2: 1
3: 7
4: 4
5: 2, 3, 5
6: 6, 9, 0
7: 8

numers each letter uses:
a: 02356789  (8, 2)
b: 045689    (6, 4)
c: 10234789  (8, 2)
d: 2345689   (7, 3)
e: 0268      (4, 6)
f: 013456789 (9, 1)
g: 0235689   (7, 3)

know: one, four, seven
A: seven - four
B: whichever character in 4 is found in 6 numbers
C: whichever character in 1 is found in 8 numbers
D: four - one - B
E: whichever character is found in 4 numbers
F: whichever character is found in 9 numbers
G: the remaining character
-}