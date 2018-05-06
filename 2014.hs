-- solutions to most haskell questions of 2014 paper

mergeSort :: ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort merge xs
        | length xs < 2 = xs
        | otherwise = merge (mergeSort merge first) (mergeSort merge second)
        where first = take half xs 
              second = drop half xs 
              half = length xs `div` 2

matches :: (Eq a) => a -> [a] -> [a]
matches _ [] = []
matches x (y:ys)
                | x == y = x : matches x ys
                | otherwise = matches x ys

matches :: (Eq a) => a -> [a] -> [a]
matches1 x ys = [ y | y <- ys, x == y]

