-- Q1
questionOne :: (a -> b) -> (a -> Bool) -> [a] -> [b]
questionOne f p xs = map f (filter p xs)

-- Q2a
{- Question appears to be incorrect. A predicate of type (a -> Bool) cannot
   act on a list that is of type [Bool] -}
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

-- Q2b
{- Question appears to be incorrect. A predicate of type (a -> Bool) cannot
   act on a list that is of type [Bool] -}
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

-- Q2c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = []

-- Q2d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = x:xs
