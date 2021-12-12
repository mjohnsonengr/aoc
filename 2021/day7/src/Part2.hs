module Part2 where
  
import Data.List.Split
import Data.List

part2 :: [String] -> Int
part2 ls =
    let crabs = parse ls
    in minimum $ map (cost crabs) [0..(maximum crabs)]

parse :: [String] -> [Int]
parse lns = sort . map strToInt . splitOn "," $ head lns

strToInt :: String -> Int 
strToInt = read

cost :: [Int] -> Int -> Int
cost crabs to = sum $ map (fuel to) crabs

fuel :: Int -> Int -> Int
fuel to from = 
    let n = abs $ from - to
    in fromIntegral $  n * (n+1) `div` 2
