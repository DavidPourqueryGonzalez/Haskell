mult :: Num a => [a] -> a
mult xs = foldr (*) 1 xs

posList :: [Int] -> [Int]
posList xs = filter(\n -> n > 0) xs

trueList :: [Bool] -> Bool
trueList bs = foldr (&&) True bs

evenList :: Integral a => [a] -> Bool
evenList xs = trueList ( map (\n -> even n) xs )

maxList :: (Ord a) => [a] -> a
maxList [] = error "no max"
maxList [x] = x
maxList (x:xs)
        | x > maximum xs = x
        | otherwise = maximum xs
   
inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = [ x | x<-xs, x >= a, x <= b]

countPositives :: [Int] -> Int 
countPositives xs = length $ posList xs

myLength :: [a] -> Int
myLength xs = foldr (+) 0 $ map (\n -> 1) xs

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = foldr (\y ys -> (f y):ys) [] xs
