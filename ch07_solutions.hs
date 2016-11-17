import Data.Char

type Bit = Int

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

-- Q3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

-- Q4
{- 1000*2 + 100*3 + 10*4 + 1*5
   10(100*2 + 10*3 + 1*4) + 1*5
   10(10(10*2 + 1*3) + 1*4) + 1*5
   10(10(10(10*0 + 1*2) + 1*3) + 1*4) + 1*5
   10(10(10(10*0 +  y ) +  y ) +  y ) +  y
-}
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- Q5
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y

-- Q6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
    | p x       = []
    | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (null) (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold (null) (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

-- Q7
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Compute the parity bit
computeParity :: [Bit] -> Bit
computeParity bits = sum bits `mod` 2

-- Append parity bit to bit list
addParityBit :: [Bit] -> [Bit]
addParityBit bits = (computeParity bits) : bits

-- Check parity bit
checkParityBit :: [Bit] -> [Bit]
checkParityBit (x:xs)
    | x == (computeParity xs) = xs
    | otherwise               = error "Parity failure!"

-- Add parity bit during the encode phase
encode :: String -> [Bit]
encode = concat . map (addParityBit . make8 . int2bin . ord)

-- Check parity bit during the decode phase
decode :: [Bit] -> String
decode =  map (chr . bin2int . checkParityBit) . chop9

-- Chopping 9 bits instead of 8 because of parity bit
chop9 :: [Bit] -> [[Bit]]
chop9 = unfold (null) (take 9) (drop 9)

-- Q8
faultyChannel :: [Bit] -> [Bit]
faultyChannel bits = tail bits

-- Call this instead of 'tramsit' to test failure case
faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode

-- Q9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []       = []
altMap f g (x:[])   = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs

-- Q10
luhnDouble :: Int -> Int
luhnDouble x = if n < 10 then n else n - 9
               where n = x*2

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0
