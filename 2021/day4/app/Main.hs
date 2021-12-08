module Main where

import Part1
import Part2
import System.IO

main :: IO ()
main = do
  p1
  p2

p1 = do
  putStrLn "Part 1"
  main' part1 "test.txt"
  main' part1 "input.txt"

p2 = do
  putStrLn "Part 2"
  main' part2 "test.txt"
  main' part2 "input.txt"

main' :: ([String] -> Int) -> String -> IO ()
main' sln file = do
  putStrLn $ "File: " ++ file
  ls <- getLines file
  print . sln $ ls

getLines :: String -> IO [String]
getLines file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  return $ lines contents
