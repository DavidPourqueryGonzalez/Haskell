-- solutions to haskell questions of 2017 paper

factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n < 0 = error "can't find factorial of negative number"
            | otherwise = n * factorial (n-1)

sumFac :: Integer -> Integer
sumFac 0 = 1
sumFac n = factorial n + sumFac (n-1)

specialList = [ (x) * (x) | x <- [1..10], (x) * (x) * (x) < 1000, (x) * (x) * (x) > 200]

negSum :: [Integer] -> Integer
negSum xs = foldr (+) 0 [ (x) * (x) | x <- xs, x < 0]

divide :: Integer -> Integer -> Bool
divide _ 1 = True
divide a b | a `mod` b == 0 = False
           | otherwise = divide a (b-1)

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = divide n (n-1) 

prodInts :: IO Int
prodInts = do 
            input <- getLine
            let value = read input :: Int
            if value /= 0
                then do
                    newValue <- prodInts
                    return (value * newValue)
            else do
                return 1


