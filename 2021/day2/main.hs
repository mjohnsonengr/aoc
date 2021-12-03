import Data.List

main :: IO ()
main = do
  input <- getContents
  let (x,y) = foldl parse (0, 0) $ lines input
  print $ x*y

parse :: (Int, Int) -> String -> (Int, Int)
parse coords statement =
  let [command, n] = words statement
      n2 = read n :: Int
  in run command n2 coords

run :: String -> Int -> (Int, Int) -> (Int, Int)
run "forward" n (x, y) = (x+n, y)
run "down" n (x, y) = (x, y+n)
run "up" n (x, y) = (x, y-n)
run _ _ _ = error("doesn't happen")
