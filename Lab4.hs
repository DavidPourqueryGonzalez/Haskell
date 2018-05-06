import Test.QuickCheck

halfEvens :: [Int] -> [Int]
halfEvens xs = [if x `mod` 2 == 0 then (div x 2) else x| x <- xs]

prop_halfEvens :: [Int] -> Bool
prop_halfEvens xs =
     sum (halfEvens xs) <= sum xs


inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = [ x | x <- xs, x >= a, x <= b]


    
