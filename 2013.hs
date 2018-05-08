import Data.List

maximum1 :: (Ord a) => [a] -> a
maximum1 [] = error "no maximum of an empty list"
maximum1 [x] = x
maximum1 (x:xs) | x > maxTail = x
               | otherwise = maxTail
               where maxTail = maximum1 xs

sameHead :: (Eq a) => [a] -> [a] -> Bool
sameHead _ [] = False
sameHead [] _ = False
sameHead (x:xs) (y:ys) | x == y = True
                       | otherwise = False

biggerThan :: (Ord a) => a -> [a] -> [a]
biggerThan _ [] = []
biggerThan x (y:ys) | x < y = y : biggerThan x ys
                    | otherwise = biggerThan x ys

sameList :: (Eq a) => [a] -> [a] -> Bool
sameList _ [] = False
sameList [] _ = False
sameList [] [] = True
sameList (x:xs) (y:ys) | x == y = sameList xs ys
                       | otherwise = False

sumPosSqr :: [Int] -> Int
sumPosSqr xs = foldr (+) 0 (map sqr (filter pos xs))
             where
                sqr x = x * x
                pos x = x > 0



absDiff :: Float -> Float -> Float
absDiff x y | x == y = 0
            | x > y = x - y
            | otherwise = y - x

nearEq :: Float -> Float -> Float -> Bool
nearEq a x y | a < absDiff x y = False
             | otherwise = True