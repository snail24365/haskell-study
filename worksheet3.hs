factorial 0 = 1
factorial n = n * factorial (n-1)

productC :: [Int] -> Int
productC [] = 1
productC (x:xs) = x * productC xs

lengthC :: [a] -> Int
lengthC [] = 0
lengthC (x:xs) = 1+ lengthC xs

reverseC :: [a] -> [a]
reverseC [] = []
reverseC (x:xs) = reverse(xs) ++ [x]

zipC :: [a] -> [b] -> [(a, b)]
zipC [] _ = []
zipC _ [] = []
zipC (x:xs) (y:ys) = (x,y) : zipC xs ys

dropC :: Int -> [a] -> [a]
dropC 0 xs = xs
dropC _ [] = []
dropC n (x:xs) = dropC (n-1) xs

{-
(++) :: [a] -> [a] -> [a]
[] ++ ys =ys
(x:xs) ++ ys = x : (xs ++ ys)
-}

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) =  qsort [x' | x' <- xs, x' < x] ++ [x] ++ qsort [x' | x' <- xs, x' >= x]

{-
even :: Int -> Bool
even 0 = True
even n = odd (n-1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n-1)


evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs


init :: [a] -> [a]
init (x:xs) | null xs = []
            | otherwise = x : init xs
-}
{-
(*) Int -> Int -> Int
m * 0 = 0
m * n = m + (m * (n-1))
-}

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x > y = y : insert x ys
                 | otherwise = x : y : ys

isort :: Ord a => [a] -> [a]
isort [] =[]
isort (x:xs) = insert x (isort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                       | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where n = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort xs = merge (msort left) (msort right)
              where (left, right) = halve xs


twice :: (a->a) -> a -> a
twice f x = f (f x)


{-
sum [] = 0
sum (x:xs) = x sum xs
-}

{-

foldr (x:xs) 

-}