module Part1 where

import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Sets

part1 :: [String] -> Int
part1 lns = sum $ map parse lns

parse :: String -> Int
parse ln =
  let code = last . splitOn ["|"] $ splitOn " " ln
      uniqueLens = Sets.fromList [2,3,4,7]
  in length $ filter (`Sets.member` uniqueLens) $ map length code

{-
0: abcefg
1: cf      -- 2
2: acdeg   -- 5
3: acdfg   -- 5
4: bcdf    -- 4
5: abdfg   -- 5
6: abdefg  -- 6
7: acf     -- 3
8: abcdefg -- 7
9: abcdfg  -- 6

how many lit: which numbers
2: 1
3: 7
4: 4
5: 2, 3, 5
6: 6, 9
7: 8
-}