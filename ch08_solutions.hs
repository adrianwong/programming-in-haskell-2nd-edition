-- Q1
data Nat = Zero | Succ Nat

add' :: Nat -> Nat -> Nat
add' Zero n     = n
add' (Succ m) n = Succ (add' m n)

mult' :: Nat -> Nat -> Nat
mult' m Zero     = Zero
mult' m (Succ n) = add' m (mult' m n)

-- Q2
data Tree a = Leaf a | Node (Tree a) a (Tree a)
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                             LT -> occurs x l
                             EQ -> True
                             GT -> occurs x r

-- Q3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

numLeaves :: Tree' a -> Int
numLeaves (Leaf' x)   = 1
numLeaves (Node' l r) = numLeaves l + numLeaves r

balanced :: Tree' a -> Bool
balanced (Leaf' x)   = True
balanced (Node' l r) = abs (numLeaves l - numLeaves r) <= 1 && balanced l && balanced r

-- Q4
halveList :: [a] -> ([a],[a])
halveList xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs  = Node' (balance ys) (balance zs)
              where (ys,zs) = halveList xs

-- Q5
data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Q6
{- test:
   eval (Add (Val 2) (Val 7)) == 9
   eval (Add (Add (Val 2) (Val 7)) (Val 9)) == 18
   eval (Add (Add (Val 2) (Val 7)) (Add (Val 3) (Val 9))) == 21
-}
eval :: Expr -> Int
eval x = folde id (+) x

{- test:
   size (Add (Val 2) (Val 7)) == 2
   size (Add (Add (Val 2) (Val 7)) (Val 9)) == 3
   size (Add (Add (Val 2) (Val 7)) (Add (Val 3) (Val 9))) == 4
-}
size :: Expr -> Int
size x = folde (\_ -> 1) (+) x
