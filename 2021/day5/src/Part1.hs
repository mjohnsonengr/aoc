module Part1 where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Maps

part1 :: [String] -> Int
part1 lns = Maps.size . Maps.filter (>=2) . getAllPoints $ parseAll lns

data Point = Point Int Int deriving (Show, Eq, Ord)
data Line = Line Point Point deriving (Show, Eq, Ord)

parseAll :: [String] -> [Line]
parseAll = map parse

parse :: String -> Line
parse ln =
    let [[x1, y1],[x2, y2]] = map (map read . splitOn ",") $ splitOn " -> " ln :: [[Int]]
        point1 = Point x1 y1
        point2 = Point x2 y2
    in Line (min point1 point2) (max point1 point2)

getAllPoints :: [Line] -> Map Point Int
getAllPoints lns = foldl (\acc ln -> Maps.unionWith (+) acc $ getPoints ln) Maps.empty lns

getPoints :: Line -> Map Point Int
getPoints (Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = Maps.fromList $ map (\y -> ((Point x1 y), 1)) [y1..y2]
  | y1 == y2 = Maps.fromList $ map (\x -> ((Point x y1), 1)) [x1..x2]
  | otherwise = Maps.empty
