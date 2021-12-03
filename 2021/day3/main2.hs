import Data.Char
import Data.List
import Data.Function

main :: IO ()
main = do
  input <- getContents
  print . sln $ lines input

sln :: [String] -> Int
sln ls = (eliminate gamma ls) * (eliminate epsilon ls)

eliminate :: ([String] -> String) -> [String] -> Int
eliminate crit ls = toDec $ eliminate' 0 crit ls

eliminate' :: Int -> ([String] -> String) -> [String] -> String
eliminate' _ _ (x:[]) = x
eliminate' n crit xs = eliminate' (n+1) crit (filter ((take n (crit xs)) `isPrefixOf`) xs)

gamma :: [String] -> String
gamma = map (head . head . gamma' . group . sortDesc) . transpose

epsilon :: [String] -> String
epsilon = map (head . head . epsilon' . group . sort) . transpose

gamma' = sortBy . flip $ (compare `on` length)
epsilon' = sortBy $ (compare `on` length)

sortDesc = sortBy (flip compare)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
