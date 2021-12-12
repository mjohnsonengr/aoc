module Part1 where

import Data.List.Split
import Data.List

part1 :: [String] -> Int
part1 ls = 
  let crabs = parse ls
  in minimum $ map (cost crabs) [0..(maximum crabs)]

parse :: [String] -> [Int]
parse lns = sort . map strToInt . splitOn "," $ head lns

strToInt :: String -> Int 
strToInt = read

cost :: [Int] -> Int -> Int
cost crabs to = sum $ map (abs . (to-)) crabs
