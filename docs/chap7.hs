-- Exercises for Chapter 7

-- Imports
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Use all" #-}
import Data.Char

-- Exercise 1 - Showing [f x | x <- xs, p x] can be rewritten with map and filter

-- [f x | x <- xs, p x] = map f (filter p xs)

-- Exercise 2 - Defining higher-order library list functions

all' :: (a -> Bool) -> [a] -> Bool
all' f = and . map f

any' :: (a -> Bool) -> [a] -> Bool
any' f = or . map f

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x y -> if f x then x : y else []) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = xs

-- Exercise 3 - Redefine map f and filter p using foldr

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) [] 

-- Exercise 4 - Define dec2int using foldl

multTen :: Int -> Int
multTen x = x * 10

dec2int :: [Int] -> Int
dec2int = foldl ((+) . multTen) 0

-- Exercise 5 - Define curry and uncurry

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y

-- Exercise 6 - Redefine chop8, map f and iterate f using unfold

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8' :: [a] -> [[a]]
chop8' = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> [a] -> [a]
iterate' f = unfold null head (map'' f . tail)

-- Exercise 7 - Add parity bit checking to the binary string transmitter

--From book:

type Bit = Int
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop9' :: [a] -> [[a]]
chop9' = unfold null (take 9) (drop 9)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

channel :: [Bit] -> [Bit]
channel = id

-- End of book content for exercise 7

encode :: String -> [Bit]
encode = concatMap (addParity . make8 . int2bin . ord)

parity :: [Bit] -> Bit
parity bits | odd (count 1 bits) = 1
            | otherwise          = 0

addParity :: [Bit] -> [Bit]
addParity bits = reverse(parity bits : reverse bits) 

count :: Eq a => a -> [a] -> Int
count x = foldr (\y -> if x == y then (+1) else (+0) ) 0

transmit :: String -> String
transmit = decode . channel . encode

decode :: [Bit] -> [Char]
decode bits | all checkParity choppedBits =  map (chr . bin2int . init) choppedBits
            | otherwise                     =  error "Parity Error"
            where choppedBits = chop9' bits


checkParity :: [Bit] -> Bool
checkParity bits = even (count 1 bits) && length bits == 9

-- Exercise 8 - Testing new transmit

transmitTest :: String -> String
transmitTest = decode . tail . channel . encode

-- Exercise 9 - Define altMap

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ [x0] = [f x0]
altMap f g (x0:x1:xs) = f x0 : g x1 : altMap f g xs

-- Exercise 10 - Define luhn using altMap

luhn :: [Int] -> Bool
luhn xs = sum (altMap (*1) luhnDouble (reverse xs)) `mod` 10 == 0

luhnDouble :: Int -> Int
luhnDouble x | y > 9     = y - 9
             | otherwise = y
             where y = x*2
