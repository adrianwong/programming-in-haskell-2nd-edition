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
-- instance Functor ((->) r) where
--     -- fmap :: (a -> b) -> f a -> f b
--     -- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
--     -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
--     fmap = (.)

-- Q3
-- Commenting due to duplicate instance declarations
-- instance Applicative ((->) r) where
--     -- pure :: a -> f a
--     -- pure :: a -> ((->) r a)
--     -- pure :: a -> (r -> a)
--     -- pure :: a -> r -> a
--     pure = const
--
--     -- (<*>) :: f (a -> b) -> f a -> f b
--     -- (<*>) :: ((->) r (a -> b)) -> ((->) r a) -> ((->) r b)
--     -- (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
--     -- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
--     (<*>) f g = \r -> f r (g r)

-- Q4
newtype ZipList a = Z [a] deriving Show
instance Functor ZipList where
    fmap g (Z xs) = Z (fmap g xs)
instance Applicative ZipList where
    pure x = Z (repeat x)
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

-- Q5
-- TODO

-- Q6
-- Commenting due to duplicate instance declarations
-- instance Monad ((->) r) where
--     -- (>>=) :: m a -> (a -> m b) -> m b
--     -- (>>=) :: ((->) r a) -> (a -> ((->) r b)) -> ((->) r b)
--     -- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
--     -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
--     (>>=) f g = \r -> g (f r) r

-- Q7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
    deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var x)   = Var (f x)
    fmap _ (Val x)   = Val x
    fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    (<*>) (Var f) x = fmap f x
    (<*>) _ (Val x) = Val x

instance Monad Expr where
    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (>>=) (Var x) f = f x
    (>>=) (Val x) _ = Val x

-- Q8
type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    -- fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))
    -- fmap g st = st >>= return . g
    fmap g st = do s <- st
                   return $ g s

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    -- stf <*> stx = S (\s ->
    --     let (f, s') = app stf s
    --         (x, s'') = app stx s' in (f x, s''))
    stf <*> stx = do f <- stf
                     x <- stx
                     return $ f x

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s ->
        let (x,s') = app st s in app (f x) s')
