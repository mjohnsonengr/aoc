main :: IO ()
main = do
  input <- getContents
  let (x,y,a) = foldl parse (0, 0, 0) $ lines input
  print $ x*y

parse :: (Int, Int, Int) -> String -> (Int, Int, Int)
parse coords statement =
  let [command, n] = words statement
      n2 = read n :: Int
  in run command n2 coords

run :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
run "forward" n (x, y, a) = (x+n, y+(n*a), a)
run "down" n (x, y, a) = (x, y, a+n)
run "up" n (x, y, a) = (x, y, a-n)
run _ _ _ = error("doesn't happen")
