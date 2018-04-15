import Data.Char

inRange :: Int -> Int -> [Int] -> [Int]
inRange _ _ [] = []
inRange a b (x : xs) = if x >= a && x <=b then x : inRange a b xs
                       else inRange a b xs

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x : xs) = if x > 0 then 1 + countPositives xs
                          else countPositives xs

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x : xs) = if a <= x then a : x : xs
                    else x : insert a xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) = if x < y then x : merge xs (y : ys)
                          else y : merge (x : xs) ys

msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort xs = if even (length xs) then merge firstHalf lastHalf
           else merge firstHalf lastHalf'
           where
           half  = (length xs) `div` 2
           half' = 1 + half
           firstHalf  = msort $ take half  xs
           lastHalf   = msort $ drop half  xs
           lastHalf'  = msort $ drop half' xs
