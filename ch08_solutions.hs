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
data Expr' = Val' Int | Add' Expr' Expr'
folde :: (Int -> a) -> (a -> a -> a) -> Expr' -> a
folde f g (Val' x)   = f x
folde f g (Add' x y) = g (folde f g x) (folde f g y)

-- Q6
{- test:
   eval (Add (Val 2) (Val 7)) == 9
   eval (Add (Add (Val 2) (Val 7)) (Val 9)) == 18
   eval (Add (Add (Val 2) (Val 7)) (Add (Val 3) (Val 9))) == 21
-}
eval' :: Expr' -> Int
eval' x = folde id (+) x

{- test:
   size (Add (Val 2) (Val 7)) == 2
   size (Add (Add (Val 2) (Val 7)) (Val 9)) == 3
   size (Add (Add (Val 2) (Val 7)) (Add (Val 3) (Val 9))) == 4
-}
size :: Expr' -> Int
size x = folde (\_ -> 1) (+) x

-- Q7
-- instance Eq a => Eq (Maybe a) where
--     Nothing  == Nothing  = True
--     (Just x) == (Just y) = x == y
--     _        == _        = False
--
-- instance Eq a => Eq [a] where
--     []     == []     = True
--     (x:xs) == (y:ys) = x == y && xs == ys
--     xs     == ys     = False

-- Q8
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          -- Extension for logical disjunction and equivalence:
          | Or Prop Prop
          | Equiv Prop Prop

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
-- Extension for logical disjunction and equivalence:
eval s (Or p q)    = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
-- Extension for logical disjunction and equivalence:
vars (Or p q)    = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

p1 = And (Var 'A') (Not (Var 'A'))
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- Q9
data Expr = Val Int | Add Expr Expr | Mult Expr Expr
type Cont = [Op]
data Op = EVALADD Expr
        | ADD Int
        -- Extension for multiplication:
        | EVALMULT Expr
        | MULT Int

eval'' :: Expr -> Cont -> Int
eval'' (Val n)    c = exec c n
-- Extension for multiplication:
eval'' (Add x y)  c = eval'' x (EVALADD y : c)
eval'' (Mult x y) c = eval'' x (EVALMULT y : c)

exec :: Cont -> Int -> Int
exec []               n = n
-- Extension for multiplication:
exec (EVALADD y : c)  n = eval'' y (ADD n : c)
exec (EVALMULT y : c) n = eval'' y (MULT n : c)
exec (ADD n : c)      m = exec c (n+m)
exec (MULT n : c)     m = exec c (n*m)

value :: Expr -> Int
value e = eval'' e []
