-- Exercises for Chapter 1

-- Exercise 1

{-
   double (double 2)
   double (2 + 2)
   2+2 + 2+2
   4 + 2+2
   4 + 4
   8
-}

-- Exercise 2

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = Main.sum xs + x

{-
   Using definition of sum above:
   sum [x]
   sum [] + x
   0 + x
   x
   Therefore sum [x] = x for all x
-}

-- Exercise 3

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = Main.product xs * x

-- Exercise 4

qsortRev :: Ord a => [a] -> [a]
qsortRev [] = []
qsortRev (x:xs) = qsortRev bigger ++ [x] ++ qsortRev smaller
               where
                smaller = [a | a <- xs, a <= x]
                bigger = [b | b <- xs, b > x]

-- Exercise 5

-- Replacing <= in the definition of qsort would mean that any duplicates of the selected x would be ignored for the sorted version of that list.