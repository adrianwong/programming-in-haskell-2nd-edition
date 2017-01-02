module ProgrammingInHaskell_Ch03 where

-- Q3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Q5
{-
An argument could have an infinite number of values. Checking that two
functions return identical results for an infinite range is not feasible.
This, however, is feasible for functions with a limited range, such as a
boolean or an 8-bit integer.
-}
