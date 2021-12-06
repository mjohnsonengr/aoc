module Main where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Part1
import Part2
import System.IO

main :: IO ()
main = do
  main' part1 "test.txt"
  main' part1 "input.txt"
  main' part2 "test.txt"
  main' part2 "input.txt"

part1' = do
  main' part1 "test.txt"
  main' part1 "input.txt"

part2' = do
  main' part2 "test.txt"
  main' part2 "input.txt"

main' :: ([String] -> Int) -> String -> IO ()
main' sln file = do
  print $ "File: " ++ file
  ls <- getLines file
  print . sln $ ls

getLines :: String -> IO [String]
getLines file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  return $ lines contents
