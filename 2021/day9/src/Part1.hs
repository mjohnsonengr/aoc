module Part1 where

import Data.Char (digitToInt)
import Data.List
import Data.List.Split

part1 :: [String] -> Int
part1 ls = 0 -- TODO

parse :: [String] -> [[Int]]
parse = map (map digitToInt)