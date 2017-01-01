import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Ord (comparing)

data Op = Add | Sub | Mul | Div | Exp -- Q6a

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^" -- Q6a

valid :: Op -> Int -> Int -> Bool

-- Use for original (optimised) program
-- valid Add x y = x <= y
-- valid Sub x y = x > y
-- valid Mul x y = x /= 1 && y /= 1 && x < y
-- valid Div x y = y /= 1 && x `mod` y == 0

-- Use for Q4 (Part 2)
-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y == 0

-- Use for Q5
-- valid Add _ _ = True
-- valid Sub _ _ = True
-- valid Mul _ _ = True
-- valid Div x y = y /= 0 && x `mod` y == 0

-- Use for Q6a
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x < y
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = x /= 1 && y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y -- Q6a

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                           brak (Val n) = show n
                           brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
-- choices = concat . map perms . subs
-- Q1
choices xs = [zs | ys <- subs xs, zs <- perms ys]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
    elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split []  = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                 l       <- exprs ls,
                 r       <- exprs rs,
                 e       <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
-- ops = [Add,Sub,Mul,Div]
ops = [Add,Sub,Mul,Div,Exp] -- Q6a

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,
                      lx     <- results ls,
                      ry     <- results rs,
                      res    <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
    [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int]-> Int -> [Expr]
solutions' ns n =
    [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- Q2
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys
    && isChoice xs (removeFirstOccurrence x ys)

removeFirstOccurrence :: Eq a => a -> [a] -> [a]
removeFirstOccurrence x [] = []
removeFirstOccurrence x (y:ys)
    | x == y    = ys
    | otherwise = y : removeFirstOccurrence x ys

-- Q3
-- Function will not terminate?

-- Q4 (Part 1)
allExprs :: [Int] -> [Expr]
allExprs ns = [e | ns' <- choices ns, e <- exprs ns']

possibleExprs :: [Int] -> Int
possibleExprs = length . allExprs

-- Q4 (Part 2) and Q5
-- Same function is called, but definition of 'valid' is different
-- This one takes a while to compute!
successfulExprs :: [Int] -> Int
successfulExprs = length . filter (not . null) . map eval . allExprs

-- Q6b
{- Helper function that calculates the value
difference between the expression and the number 'n'
and stores the expression and its difference in a tuple -}
calcExprDiffTuple :: [Int] -> Int -> [(Expr,Int)]
calcExprDiffTuple ns n = [(e, abs (m-n)) | ns' <- choices ns, (e,m) <- results ns']

lowestDiffSolns :: [(Expr,Int)] -> [(Expr,Int)]
lowestDiffSolns = concat
                 . take 1 -- Take first element, which will be lowest diff group
                 . groupBy (on (==) snd) -- Group diffs of same value
                 . sortBy (comparing snd) -- Sort by increasing order of diffs

closestSolns :: [Int] -> Int -> [Expr]
closestSolns ns n = map fst (lowestDiffSolns (calcExprDiffTuple ns n))

-- Q6c
-- Complexity determined by assigning a Fibonacci value to an operation
exprComplexity :: Expr -> Int
exprComplexity (Val n)       = 0
exprComplexity (App Add l r) = 1 + exprComplexity l + exprComplexity r
exprComplexity (App Sub l r) = 2 + exprComplexity l + exprComplexity r
exprComplexity (App Mul l r) = 3 + exprComplexity l + exprComplexity r
exprComplexity (App Div l r) = 5 + exprComplexity l + exprComplexity r
exprComplexity (App Exp l r) = 8 + exprComplexity l + exprComplexity r

sortedClosestSolns :: [Int] -> Int -> [Expr]
sortedClosestSolns ns n = sortBy (comparing exprComplexity) (closestSolns ns n)
