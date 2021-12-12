module Part2 where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Maps

part2 :: [String] -> Int
part2 lns = Maps.size . Maps.filter (>=2) . getAllPoints $ parseAll lns

data Point = Point Int Int deriving (Show, Eq, Ord)
data Line = Line Point Point deriving (Show, Eq, Ord)

parseAll :: [String] -> [Line]
parseAll = map parse

parse :: String -> Line
parse ln =
    let [[x1, y1],[x2, y2]] = map (map read . splitOn ",") $ splitOn " -> " ln :: [[Int]]
    in Line (Point x1 y1) (Point x2 y2)

getAllPoints :: [Line] -> Map Point Int
getAllPoints lns = foldl (\acc ln -> Maps.unionWith (+) acc $ getPoints ln) Maps.empty lns

getPoints :: Line -> Map Point Int
getPoints (Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = Maps.fromList $ map (\y -> ((Point x1 y), 1)) $ range y1 y2
  | y1 == y2 = Maps.fromList $ map (\x -> ((Point x y1), 1)) $ range x1 x2
  | otherwise = Maps.fromList $ map (\(x,y) -> (Point x y, 1)) $ zip (range x1 x2) (range y1 y2)

range :: Int -> Int -> [Int]
range x y
  | x <= y = [x..y]
  | x > y = [x,x-1..y]
