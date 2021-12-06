import System.IO
import Data.Char
import Data.List
import Data.Function
import Data.List.Split

main :: IO ()
main = do
  main' "test.txt"
  main' "input.txt"

main' :: String -> IO ()
main' file = do
  print $ "File: " ++ file
  ls <- getLines file
  print . sln $ ls

getLines :: String -> IO [String]
getLines file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  return $ lines contents


sln :: [String] -> Int
sln ls =
  let ls' = splitOn [""] ls
      nums = head ls'
      bs = getBoards $ tail ls'
  in error("not implemented yet")

getBoards :: [[String]] -> [[[Int]]]
getBoards = map ((map (map read)) . map words) . tail


applyNum :: Int -> [[[Int]]] -> [[[Int]]]
applyNum num bs = map (applyNum' num) bs

applyNum' :: Int -> [[Int]] -> [[Int]]
applyNum' num b = map (applyNum'' num) b

applyNum'' :: Int -> [Int] -> [Int]
applyNum'' num row = map (\n -> if n==num then -1; else n) row

boardWins :: [[Int]] -> Bool
boardWins b = (any rowWin b) || (any rowWin $ transpose b)

rowWin :: [Int] -> Bool
rowWin = all (-1==)

calcScore :: [[Int]] -> Int
calcScore b = sum . filter (-1/=) $ concat b
