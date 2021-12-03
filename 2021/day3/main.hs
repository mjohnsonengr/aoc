import Data.Char
import Data.List
import Data.Function

main :: IO ()
main = do
  input <- getContents
  print . sln $ lines input

sln :: [String] -> Int
sln ls = (gamma ls) * (epsilon ls)

gamma = toDec . map (head . head . gamma' . group . sort) . transpose
epsilon = toDec . map (head . head . epsilon' . group . sort) . transpose

gamma' = sortBy . flip $ (compare `on` length)
epsilon' = sortBy $ (compare `on` length)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
