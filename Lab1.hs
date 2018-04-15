import Data.Char

square :: Int -> Int
square x = x * x

pyth :: Int -> Int -> Int
pyth x y = square x + square y

isTriple :: Int -> Int -> Int -> Bool
isTriple x y z | pyth x y == square z = True
               | otherwise = False

isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny x y z | pyth x y == square z = True
                  | pyth x z == square y = True
                  | pyth y y == square x = True
                  | otherwise = False

halfEvens :: [Int] -> [Int]
halfEvens xs = [if x `mod` 2 == 0 then x `div` 2 else x | x <- xs]

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = [ x | x <- xs, x >= a, x <= b]

countPositives :: [Int] -> Int
countPositives xs = sum [1 | x <- xs, x > 0]

capitalised :: String -> String
capitalised (x : xs) = toUpper x : [toLower y | y <- xs]

stringLower :: String -> String
stringLower xs = [ toLower x | x <- xs ]

title :: [String] -> [String]
title [] = []
title (xs:xss) = capitalised xs : [ if length ys > 4 then capitalised ys else stringLower ys | ys <- xss]
