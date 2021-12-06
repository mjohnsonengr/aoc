{-# LANGUAGE TupleSections #-}
module Part2
    ( part2
    ) where

import Data.List
import Data.List.Split

part2 :: [String] -> Int
part2 ls =
  let nums = map strToInt . splitOn "," $ head ls
      bs = getBoards . tail $ splitOn [""] ls
  in sln' nums bs Nothing

sln' :: [Int] -> [[[Int]]] -> Maybe ([[Int]], Int) -> Int
sln' _ _ (Just (winner, lastNum)) = calcScore winner * lastNum
sln' nums bs Nothing =
  let nums' = tail nums
      num = head nums
      bs' = filterToLast $ applyNum num bs
      maybeWinner = (, num) <$> checkBoards bs'
  in sln' nums' bs' maybeWinner

filterToLast :: [[[Int]]] -> [[[Int]]]
filterToLast bs
  | length bs == 1 = bs
  | otherwise = filter (not . boardWins) bs

strToInt :: String -> Int
strToInt = read

getBoards :: [[String]] -> [[[Int]]]
getBoards = map (map (map read . words)) . tail

applyNum :: Int -> [[[Int]]] -> [[[Int]]]
applyNum num = map (map (map (\n -> if n==num then -1; else n)))

-- returns winning board if there is one.
checkBoards :: [[[Int]]] -> Maybe [[Int]]
checkBoards = find boardWins

boardWins :: [[Int]] -> Bool
boardWins b = any rowWin b || any rowWin (transpose b)
  where rowWin = all (-1==)

calcScore :: [[Int]] -> Int
calcScore b = sum . filter (-1/=) $ concat b

