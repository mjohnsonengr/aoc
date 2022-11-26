-- TODO: Read in list from input.txt
list = [1721, 979, 366, 299, 675, 1456]
is2020 (x, y, sum) = sum == 2020
filterFirst p (x:xs)
    | p x = x
    | otherwise = filterFirst p xs
multXY (x, y, _) = x * y

-- TODO: output this
-- multXY filterFirst is2020 [(x, y, x+y) | x <- list, y <- list]