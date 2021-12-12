module Part1 where

import Data.List
import Data.List.Split

part1 :: [String] -> Int
part1 lns = length . part1' 80 $ parse lns

part1' :: Int -> [Int] -> [Int]
part1' 0 fish = fish
part1' n fish = part1' (n-1) $ incrementAll fish

parse :: [String] -> [Int]
parse lns = sort . map strToInt . splitOn "," $ head lns

strToInt :: String -> Int
strToInt = read

incrementAll :: [Int] -> [Int]
incrementAll fish = concat $ map increment fish

increment :: Int -> [Int]
increment fish
  | fish == 0 = [6, 8]
  | otherwise = [fish-1]
