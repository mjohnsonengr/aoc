module Part1
    ( part1
    ) where

import Data.List.Split

part1 :: [String] -> Int
part1 lns = 0 -- TODO

data Point = Point Int Int
data Line = Line Point Point

parseAll :: [String] -> [Line]
parseAll = map parse

parse :: String -> Line
parse ln =
    let [[x1, y1],[x2, y2]] = map (map read . splitOn ",") $ splitOn " -> " ln :: [[Int]]
    in Line (Point x1 y1) (Point x2 y2)