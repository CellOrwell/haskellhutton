-- Exercises for Chapter 2

--Example 1 - Unable to visualise here, it's a task of working through the examples in the chapters

-- Example 2

{-

1. 2^(3*4)
2. (2*3)+(4*5)
3. 2+(3*(4^5))

-}

-- Example 3

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

{-

Changes made:
'N' changed to 'n'
'div' changed to `div`
a and xs indented the same

-}

-- Example 4

last1 :: [a] -> a
last1 = head . reverse

last2 :: [a] -> a
last2 xs = head(drop (length xs - 1) xs)

last3 :: [a] -> a
last3 xs = xs !! (length xs - 1)

--Example 5

init1 :: [a] -> [a]
init1 xs = reverse(drop 1(reverse xs))

init2 :: [a] -> [a]
init2 xs = take (length xs - 1) xs
