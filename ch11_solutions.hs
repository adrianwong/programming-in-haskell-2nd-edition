module ProgrammingInHaskell_Ch11 where

import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

data Player = O | B | X
              deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
             os = length (filter (== O) ps)
             xs = length (filter (== X) ps)
             ps = concat g

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
               line = all (== p)
               rows = g
               cols = transpose g
               dias = [diag g, diag (map reverse g)]

won :: Grid -> Bool
won g = wins O g || wins X g

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave _ []     = []
interleave _ [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
              beside = foldr1 (zipWith (++))
              bar    = replicate 3 "|"

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size*4)-1) '-']

valid :: Grid -> Int -> Bool
valid g i = i >= 0 && i < size^2 && concat g !! i == B

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
             where (xs,B:ys) = splitAt i (concat g)
