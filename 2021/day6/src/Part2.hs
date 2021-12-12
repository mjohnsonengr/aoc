module Part2 where

import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Maps

part2 :: [String] -> Int
part2 lns = Maps.foldl' (+) 0 . part2' 256 $ parse lns

part2' :: Int -> Map Int Int -> Map Int Int
part2' 0 fish = fish
part2' n fish = part2' (n-1) $ incrementAll fish

parse :: [String] -> Map Int Int
parse lns = Maps.fromList . map (\lst -> (head lst, length lst)) . group . sort . map strToInt . splitOn "," $ head lns

strToInt :: String -> Int
strToInt = read

incrementAll :: Map Int Int -> Map Int Int
incrementAll fish = Maps.foldlWithKey' increment emptyMap fish

increment :: Map Int Int -> Int -> Int -> Map Int Int
increment newFish age count
  | age == 0 = Maps.adjust (+count) 6 $ Maps.adjust (+count) 8 newFish
  | otherwise = Maps.adjust (+count) (age-1) newFish

emptyMap :: Map Int Int
emptyMap = Maps.fromList $ map (\x -> (x, 0)) [0..8]
