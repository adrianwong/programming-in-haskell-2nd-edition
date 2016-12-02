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
