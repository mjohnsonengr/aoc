import Data.List

main :: IO ()
main = do
  input <- getContents
  let numbers = map read (lines input) :: [Integer]
  let sums = map sumThree $ takeWhile (\xs -> (length xs) >= 3) $ tails numbers
  print $ solution sums

sumThree (x1:x2:x3:xs) = x1+x2+x3

solution :: [Integer] -> Int
solution = snd . sol'

sol' :: [Integer] -> (Integer, Int)
sol' input = foldl isInc (head input, 0) input

isInc (prev, count) cur
  | cur > prev = (cur, count+1)
  | otherwise = (cur, count)



