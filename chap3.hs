-- Chapter 3 Exercises

-- Exercise 1

{-

['a', 'b', 'c'] :: [Char]

('a', 'b', 'c') :: (Char, Char, Char)

[(False, '0'), (True, '1')] :: [(Bool, Char)]

([False, True], ['0', '1']) :: ([a], [b])

[tail, init, reverse] :: [[a] -> [a]]

-}

-- Exercise 2
bools :: [Bool]
bools = [True, False, True]

nums :: [[Int]]
nums = [[1,2,3], [4,5]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply func x = func x

-- Exercise 3

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Exercise 4

{-

Ex 1:
- All correct

Ex 2:
- Initial GHCi pass so my function definitions are correct.

Ex 3:
- Missed out Eq class in type definition for palindrome
- Apart from that, all were correct

-}

-- Exercise 5

{-

It's not feasible for function types to be instances of the Eq class since it's not feasible to compare 2 functions to check if they're equal

It is feasible when two functions have the same exact type definitions

-}