-- Exercises for Chapter 8

-- Exercise 1 - Define recursive multiplication succession function

-- From Book
data Nat = Zero | Succ Nat
           deriving (Show)

addSucc :: Nat -> Nat -> Nat
addSucc Zero n = n
addSucc (Succ m) n = Succ (addSucc m n)

-- End of book content

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ Zero) n = n
mult (Succ m) n = addSucc (mult m n) n 

-- Exercise 2 - Redefine occurs with Ordering and compare

-- From Book
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)
-- End of book content

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of
                            LT -> occurs x r
                            GT -> occurs x l
                            EQ -> True


-- This is way more efficient since for a node, compare is only ran once.

-- Exercise 3 - Define balanced function

data BTree a = BLeaf a | BNode (BTree a) (BTree a) deriving (Show)

leafCount :: BTree a -> Int
leafCount (BLeaf x) = 1
leafCount (BNode l r) = leafCount l + leafCount r

balanced :: BTree a -> Bool
balanced (BLeaf a) = True
balanced (BNode l r) = abs (leafCount l - leafCount r)  <= 1 && balanced l && balanced r

-- Exercise 4 - Define balance function

splitMiddle :: [a] -> ([a], [a])
splitMiddle xs = splitAt listLen xs
                 where listLen = length xs `div` 2

balance :: [a] -> BTree a
balance [x] = BLeaf x
balance xs = BNode (balance ys) (balance zs) 
            where (ys, zs) = splitMiddle xs

-- Exercise 5 - Define folde

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Exercise 6 - Using folde to make eval and sizze functions

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- Exercise 7 - Complete instance declarations

-- instance Eq a => Eq (Maybe a) where
--     Nothing == Nothing = True
--     (Just x) == (Just y) = x == y
--     _ == _ = False

-- instance Eq a => Eq [a] where
--     [] == [] = True
--     (x:xs) == (y:ys) = x == y && xs == ys
--     _ == _ = False

-- Exercise 8 - Extend tautology checker to support logical disjunction and equivalence

--From book

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) xs

isTaut :: Prop -> Bool
isTaut p = and [eval' s p | s <- substs p]

-- End of book content

data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
            | Imply Prop Prop
            | Equiv Prop Prop

eval' :: Subst -> Prop -> Bool
eval' _ (Const b) = b
eval' s (Var x) = find x s
eval' s (Not p) = not (eval' s p)
eval' s (And p q) = eval' s p && eval' s q
eval' s (Or p q) = eval' s p || eval' s q
eval' s (Imply p q) = eval' s p <= eval' s q
eval' s (Equiv p q) = eval' s p == eval' s q

-- Exercise 9 - Extend abstract machine to support multiplication use

-- From book

type Cont = [Op]

data Op = ADD ExprNew | MULT ExprNew | ADDON Int | MULTON Int

value :: ExprNew -> Int
value e = eval'' e []

-- End of book content

data ExprNew = NVal Int | NAdd ExprNew ExprNew | NMult ExprNew ExprNew

-- value :: ExprNew -> Int
-- value (NVal n) = n
-- value (NAdd x y) = value x + value y
-- value (NMult x y) = value x * value y

eval'' :: ExprNew -> Cont -> Int
eval'' (NVal n) c = exec c n
eval'' (NAdd x y) c = eval'' x (ADD y : c)
eval'' (NMult x y) c = eval'' x (MULT y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (ADD n : c) m = eval'' n (ADDON m : c)
exec (MULT n : c) m = eval'' n (MULTON m : c)
exec (ADDON m : c) n = exec c (n+m)
exec (MULTON m : c) n = exec c (n*m)