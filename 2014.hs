-- solutions to haskell questions of 2014 paper

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

matches1 :: (Eq a) => a -> [a] -> [a]
matches1 x ys = [ y | y <- ys, x == y]

squareSum :: (Num a) => [a] -> a
squareSum xs = foldr (+) 0 (map (\n -> n * n) xs)

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) 
              | x <= y = x : y :ys
              | otherwise = y : insert x ys

iSort :: (Ord a) => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x $ iSort xs

data Set a = Set [a] deriving Show

sum1 :: [Integer] -> Integer
sum1 [] = 0
sum1 (x:xs) = 1 + sum1 xs

sumAcc :: Integer -> [Integer] -> Integer
sumAcc acc [] = acc
sumAcc acc (x:xs) = sumAcc (x + acc) xs

