module ProgrammingInHaskell_Ch04 where

-- Q5
func :: Bool -> Bool -> Bool
func a b = if a then
               if b then True else False
           else False

-- Q6
func' :: Bool -> Bool -> Bool
func' a b = if a then b else False

-- Q7
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> (x*y*z)))

-- Q8
luhnDouble :: Int -> Int
luhnDouble x
    | n < 10    = n
    | otherwise = n-9
    where n = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = ((luhnDouble w) + x + (luhnDouble y) + z) `mod` 10 == 0
