-- Exercises for Chapter 6

-- Exercise 1 - Modify factorial function to reject negative numbers

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- If a negative number is used, the factorial function will go on forever
-- e.g. fac -1 = -1 * fac (-2)
--             = -1 * -2 * fac(-3) etc...
-- It will never reach the base case of fac(0) and so will just go forever

fac' :: Int -> Int
fac' 0 = 1
fac' n | n >= 0 = n * fac(n-1)
       | otherwise = 0

-- Exercise 2 - Define function that returns sum of n >= 0 from given value to zero

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- Exercise 3 - Define exponentiation operator

(^^^) :: Int -> Int -> Int
_ ^^^ 0 = 1
0 ^^^ _ = 1
n ^^^ m = n * (n ^^^ (m-1))

{- 
Evaluation of ^^^

2 ^^^ 3
= 2 * (2 ^^^ 2)
= 2 * (2 * (2 ^^^ 1))
= 2 * (2 * (2 * (2 ^^^ 0)))
= 2 * (2 * (2 * 1))
= 8

-}

-- Exercise 4 - Define function which implements Euclid's Algorithm

euclid :: Int -> Int -> Int
euclid 0 x = x
euclid x 0 = x
euclid n m | n == m = n
           | n < m = euclid n (m-n)
           | otherwise = euclid (n-m) m

-- Exercise 5 - Show how length [1,2,3], drop 3 [1,2,3,4,5] and init [1,2,3] are evaluated

{- 
1.
length [1,2,3]
= 1 + (length [2,3])
= 1 + (1 + length [3])
= 1 + (1 + (1 + length []))
= 1 + (1 + (1 + 0))
= 3

2.
drop 3 [1,2,3,4,5]
= drop 2 [2,3,4,5]
= drop 1 [3,4,5]
= drop 0 [4,5]
= [4,5]

3.
init [1,2,3]
= 1 : init [2,3]
= 1 : 2 : init [3]
= 1 : 2 : []
= [1,2]
-}

-- Exercise 6 - Define library functions using recursion

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

select' :: [a] -> Int -> a
select' (x:_) 0 = x
select' (x:xs) n = select' xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' c (x:xs) | c == x = True
               | otherwise = elem' c xs

-- Exercise 7 - Define a merge function, which merges 2 ordered lists into 1 ordered list, using recursion
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Exercise 8 - Define merge sort using recursion
halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
            where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
           where (ys, zs) = halve xs

{- 
msort [6,5,4,3,2,1]
= merge (msort [6,5,4]) (msort [3,2,1])
= merge (merge (msort [6]) (msort [5,4])) (merge (msort [3]) (msort [2,1]))
= merge (merge [6] (merge (msort [5]) (msort [4]))) (merge [3] (merge (msort [2]) (msort [1])))
= merge (merge [6] (merge [5] [4])) (merge [3] (merge [2] [1]))
= merge (merge [6] [4,5]) (merge [3] [1,2])
= merge [4,5,6] [1,2,3]
= merge [1,2,3,4,5,6]
-}

-- Exercise 9 - Construct library functions using 5 step process

{-
- 1. Creating sum of list of numbers

- Define the type

sum' :: [Int] -> Int

-- Enumerate cases

sum' [] =
sum' (x:xs) =

- Define simple case

sum' [] = 0

- Define other cases

sum' (x:xs) = x + sum' xs

- Generalise and Simplify
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

OR
-}
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

{- 
- 2. Take given n.o. elements from start of list

- Define type

take' :: Int -> [a] -> [a]

- Enumerate cases

take' 0 _ = 
take' _ [] = 
take' n xs = 

- Define simple case

take' 0 xs = []
take' _ [] = []

- Define other cases

take' n (x:xs) = x ++ take' (n-1) xs

- Generalise and simplify
    Nothing that I can think of doing to generalise and simplify

-}
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

{- 
- 3. Select last element of non-empty list

- Define type

last' :: [a] -> a

- Enumerate cases

last' [x] = 
last' (x:xs) =

- Define simple case

last' [x] = x

- Define other cases

last' (x:xs) = last' xs

- Generalise and Simplify
    Nothing I can think of doing to generalise and simplify
-}
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
