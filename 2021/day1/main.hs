main :: IO ()
main = do
  input <- getContents
  let numbers = map read (lines input) :: [Integer]
  print $ solution numbers

solution :: [Integer] -> Int
solution = snd . sol'

sol' :: [Integer] -> (Integer, Int)
sol' input = foldl isInc (head input, 0) input

isInc (prev, count) cur
  | cur > prev = (cur, count+1)
  | otherwise = (cur, count)
