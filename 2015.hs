-- solutions to haskell questions of 2015 paper

insert :: Integer -> [Integer] -> [Integer]
insert x [] = [x]
insert x (y:ys) 
            | x < y = x : y : ys
            | otherwise = y : insert x ys

iSort :: [Integer] -> [Integer]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

combi :: (Real a) => [(a,a)] -> [a]
combi xs = [sum[ a | x <- xs, let a = fst x, let b = snd x, a > b]]
                                    ++
            [sum[b | x <- xs , let a = fst x, let b = snd x, a > b]]

zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 f [] _ = []
zipWith1 f _ [] = []
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys

quickSort :: (Ord a) => [a] -> [a] 
quickSort [] = []
quickSort (x:xs) = quickSort [ a | a <- xs, a < x]
                           ++ [x] ++
                    quickSort [ b | b <- xs, b >= x]

curry :: ( (a, b) -> c) -> a -> b -> c
curry f a b = f (a,b)

uncurry :: ( a -> b -> c) -> ( (a, b) -> c)
uncurry f (a,b) = f a b

backwards :: IO ()
backwards = do
            putStrLn "Enter a string : "
            input <- getLine
            putStrLn "Your string reversed is " 
            putStrLn (reverse $ input)

mapFuns :: [a->b] -> a -> [b]
mapFuns [] _ = []
mapFuns (f:fs) x = f x : mapFuns fs x

fodlr1 :: (a -> b -> b) -> b -> [a] -> b
foldr1 f z [] = z
foldr1 f z (x:xs) = f x (foldr1 f z xs)





