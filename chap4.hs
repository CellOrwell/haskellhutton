--Exercises for Chapter 4

-- Exercise 1

halve :: [a] -> ([a], [a])
halve xs = (take y xs, drop y xs)
           where y = length xs `div` 2

-- Exercise 2

third1 :: [a] -> a
third1 xs = head(tail(tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_:_:x:xs) = x 

-- Exercise 3

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (x:xs) = xs

-- Exercise 4

or1 :: Bool -> Bool -> Bool
True `or1` True = True
True `or1` False = True
False `or1` True = True
False `or1` False = False

or2 :: Bool -> Bool -> Bool
False `or2` False = False
_ `or2` _ = True

or3 :: Bool -> Bool -> Bool
True `or3` _ = True
False `or3` b = b

or4 :: Bool -> Bool -> Bool
b `or4` c | b == True = True
          | c == True = True
          | otherwise = False

-- Exercise 5

andConj :: Bool -> Bool -> Bool
andConj x y = if x then
                if y then True else False
              else False

-- Exercise 6

andConj2 :: Bool -> Bool -> Bool
andConj2 x y = if x then y else False

-- Exercise 7

{- 
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

In Lambda form:

\x -> (\y -> (\z -> x * y * z))
-}

-- Exercise 8

luhnDouble :: Int -> Int
luhnDouble x = if x*2 > 9 then x*2 - 9 else x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z h = (((luhnDouble x) + y + (luhnDouble z) + h) `mod` 10) == 0
