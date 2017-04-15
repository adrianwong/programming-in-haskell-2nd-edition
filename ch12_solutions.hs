module ProgrammingInHaskell_Ch12 where

-- Q1
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> f a -> f b
    fmap _ Leaf         = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- Q2
-- Commenting due to duplicate instance declarations
-- instance Functor ((->) a) where
--     fmap = (.)

-- Q3
-- Commenting due to duplicate instance declarations
-- instance Applicative ((->) a) where
--     pure      = const
--     g (<*>) h = \x -> g x (h x)

-- Q4
newtype ZipList a = Z [a] deriving Show
instance Functor ZipList where
    fmap g (Z xs) = Z (fmap g xs)
instance Applicative ZipList where
    pure x = Z (repeat x)
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]
