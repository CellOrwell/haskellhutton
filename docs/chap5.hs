-- Exercises for Chapter 5

--Imports
import Data.Char


-- Exercise 1 - Creating function that calculates sum 1^2+2^2+...+100^2

sqrSumHun :: Int
sqrSumHun = sum [x^2 | x <- [1..100]]

-- Using list comprehension, I make a list of square numbers from 1^2 to 100^2, using the generator x <- [1..100] to cycle through 1->100.
-- The library function 'sum' is used to add all the elements of the created list together, giving us the desired result.

-- Exercise 2 - Creating function which prints all coordinates in grid of size mxn

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- Generators can be seen as nested for loops, so for every element of x, the generator y <- [0..n] will count from 0->n.
-- I then use a tuple of two integers to represent my coordinates, and use list comprehension to store every coordinate generated in my list

--Exercise 3 - Creating function which prints all coordinates in grid of size mxm except from main diagonal coordinates

square :: Int -> [(Int, Int)]
square m = [(x, y) | (x, y) <- grid m m, x /= y]

-- Here, I used the guard 'x /= y' to exclude any coordinates from my list which were in the main diagonal of the square.

--Exercise 4 - Showing how the replicate function can be defined using list comprehension

replicate' :: Int -> a -> [a]
replicate' n x = [x | m <- [0..n]]

-- Here, I'm showing that the generator value doesn't have to be used in the list, note how I don't use the assigned m value in my list.

--Exercise 5 - Creating a function which produces pythagorean triples up to a certain value.

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Here, I simply used 3 generators and 1 guard to produce all possible coordinates, and then filter them by whether they're pythagorean or not.

-- Exercise 6 - Creating a function which returns a list of perfect numbers up to a given value

factors' :: Int -> [Int]
factors' n = [x | x <- [1..n], n `mod` x == 0, n /= x]

-- The factors function is needed to make the perfects function. The definition for the factors function was given in the book.
-- I altered the factors function to add an extra guard, which means that the number being checked isn't in the list given, as per the rules of a perfect number
-- Because of this change, I'm going to rename factors to factors'

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors' x) == x]

-- Exercise 7 - Rewrite [(x,y) | x <- [1,2], y <- [3,4]] into 2 comprehensions with 1 generator each

orig :: [(Int, Int)]
orig = [(x, y) | x <- [1,2], y <- [3,4]]

rewrite :: [(Int, Int)]
rewrite = concat [[(x, y) | y <- [3,4]] | x <- [1,2]]

-- orig function used for testing purposes
-- I struggled with this one quite a bit to be honest, wasn't really sure why it wanted me to do this?

-- Exercise 8 - Redefine positions using find function

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- find function is defined in the book

positionsFind :: Eq a => a -> [a] -> [Int]
positionsFind x xs = find x (zip xs [0..])

-- Zip library function is used to pair each element of the list to an index, and then it is treated like a key pair value
-- Find then looks for all instances in our zipped array with the first element of a tuple as the element we're looking for.

-- Exercise 9 -- Create a scalar product function using list comprehension

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Exercise 10 -- Modify the Caesar Cipher program to handle upper-case letters

let2int :: Char -> Int
let2int c | isUpper c = ord c - ord 'A'
          | otherwise = ord c - ord 'A' - 6

int2let :: Int -> Char
int2let n | n >= 26 = chr(ord 'A' + 6 + n)
          | otherwise = chr(ord 'A' + n)

shift :: Int -> Char -> Char 
shift n x | isAlpha x = int2let ((let2int x + n) `mod` 52)
          | otherwise = x

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- I'd imagine this is where Graham would want us to stop, but I'm going to attempt adapting the cipher crack

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 22.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count' x xs) n | x <- ['a'..'z']]
           where n = length xs

count' :: Char -> String -> Int
count' c xs = sum [1 | x <- xs, c == x || swapCase c == x ]

swapCase :: Char -> Char
swapCase c | isLower c = int2let (let2int c - 26)
           | isUpper c = int2let (let2int c + 26)
           | otherwise = c

chisqr :: [Float] -> [Float] -> Float 
chisqr os es = sum [((o-e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positionsFind (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

-- The crack, rotate, chisqr, freqs, percent, encode, and table functions were from the book
-- let2int, int2let, shift, count functions were in the book (or a library function), but adjusted by me
-- swapCase was made entirely by me