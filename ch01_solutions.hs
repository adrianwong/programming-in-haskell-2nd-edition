module ProgrammingInHaskell_Ch01 where

-- Q4
-- Swap occurrences of 'smaller' and 'larger'
qsort []     = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                   smaller = [a | a <- xs, a <= x]
                   larger  = [b | b <- xs, b > x]

-- Q5
-- Duplicates will be removed from the sorted list
qsort' []     = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
                where
                    smaller = [a | a <- xs, a < x]
                    larger  = [b | b <- xs, b > x]
