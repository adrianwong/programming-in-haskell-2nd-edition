module ProgrammingInHaskell_Ch05 where

import Data.Char

-- Q6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

-- Q7
reexpressed :: [(Int,Int)]
reexpressed = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- Q8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x [(k,v) | (k,v) <- zip xs [0..]]

-- Q9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- Q10
let2int :: Char -> Int
let2int c = ord c - ord 'a'

let2int' :: Char -> Int
let2int' c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2let' :: Int -> Char
int2let' n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | isUpper c = int2let' ((let2int' c + n) `mod` 26)
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
