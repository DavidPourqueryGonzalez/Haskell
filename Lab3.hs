mult :: Num a => [a] -> a
mult xs = foldr (*) 1 xs
