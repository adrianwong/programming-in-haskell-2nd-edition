module ProgrammingInHaskell_Ch10 where

import Data.Char (isDigit, digitToInt)

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
-- Q2
-- putBoard = putBoard' 1
-- Q3
putBoard xs = sequence_ [putRow n x | (n,x) <- zip [1..] xs]

-- Q2
putBoard' :: Int -> Board -> IO ()
putBoard' row []     = return ()
putBoard' row (x:xs) = do putRow row x
                          putBoard' (row+1) xs

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                         return (digitToInt x)
                     else
                         do putStrLn "ERROR: Invalid digit"
                            getDigit prompt

play :: Board -> Int -> IO ()
play board player =
    do newline
       putBoard board
       if finished board then
           do newline
              putStr "Player "
              putStr (show (next player))
              putStrLn " wins!!"
       else
           do newline
              putStr "Player "
              putStrLn (show player)
              row <- getDigit "Enter a row number: "
              num <- getDigit "Stars to remove: "
              if valid board row num then
                  play (move board row num) (next player)
              else
                  do newline
                     putStrLn "ERROR: Invalid move"
                     play board player

nim :: IO ()
nim = play initial 1

-- Q1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- Q4
adder' :: Int -> Int -> IO Int
adder' t n = do x <- getDigit ""
                if n == 1 then
                    return (t+x)
                else
                    adder' (t+x) (n-1)

adder :: IO ()
adder = do n <- getDigit "How many numbers? "
           if n < 1 then
               do putStrLn "ERROR: Value has to be > 0"
                  adder
           else
               do total <- adder' 0 n
                  putStr "The total is "
                  putStrLn (show total)

-- Q5
adderSequence' :: Int -> IO [Int]
adderSequence' n = sequence (replicate n (getDigit ""))

adderSequence :: IO ()
adderSequence = do n <- getDigit "How many numbers? "
                   if n < 1 then
                       do putStrLn "ERROR: Value has to be > 0"
                          adderSequence
                   else
                       do numberList <- adderSequence' n
                          putStr "The total is "
                          putStrLn (show (sum numberList))
