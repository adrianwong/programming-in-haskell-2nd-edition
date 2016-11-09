-- Q6a
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

-- Q6b
concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

-- Q6c
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x

-- Q6d
bangbang :: [a] -> Int -> a
bangbang (x:xs) 1 = x
bangbang (x:xs) n = bangbang xs (n-1)

-- Q6e
elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs)
    | e == x    = True
    | otherwise = elem' e xs

-- Q7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Q8
halve :: [a] -> ([a],[a])
halve xs = ((take n xs), (drop n xs))
           where n  = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
            where (ys,zs) = halve xs

-- Q9a
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- Q9b
take' :: Int -> [a] -> [a]
take' 0 xs     = []
take' n (x:xs) = x : take' (n-1) xs

-- Q9c
last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs
