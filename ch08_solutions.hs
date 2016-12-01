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
