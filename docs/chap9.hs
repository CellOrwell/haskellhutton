-- Exercises for Chapter 9

-- From Book

main :: IO()
main = print (solutions' [1,3,7,10,25,50] 765)

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1
valid Div x y = x `mod` y == 0 && y /= 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                        brak (Val n) = show n
                        brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ x y) = values x ++ values y

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
             where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx       <- results ls,
                    ry       <- results rs,
                    res      <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- End of Book Content

-- Exercise 1 - Redefine choices using list comprehension

choices' :: [a] -> [[a]]
choices' xs = [x | y <- subs xs, x <- perms y]

-- Exercise 2 - Define recursive isChoice - Decides if one list is chosen from another without perms and subs

remOne :: Eq a => a -> [a] -> [a]
remOne x [] = []
remOne x (y:ys) | x == y = ys
                | otherwise = y : remOne x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice xs [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (remOne x ys)

-- Exercise 3 

-- Generalising the split function to include pairs with the empty set would mean the function would be non-terminating, since recursive calls to exprs would no longer guarantee a reduction in list length.

-- Exercise 4

possExprs :: [Int] -> [Expr]
possExprs = concat . map exprs . choices

allEval :: [Int] -> Int
allEval = length . filter (not . null) . map eval . possExprs

allExprs :: [Int] -> Int
allExprs = length . possExprs