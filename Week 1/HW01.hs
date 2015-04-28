{-# OPTIONS_GHC -Wall #-}
module HW01 where

import Data.Char

-- Exercise 1 -----------------------------------------
digitize :: String -> [Integer]
digitize [] = []
digitize (x:xs) = toInteger (digitToInt x) : digitize xs
digitize n = []

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = last (digitize (show n))

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------
--  Ex: toRevDigits 1234 == [4,3,2,1]
--  Ex: toRevDigits 0 == []
--  Ex: toRevDigits (-17) == []

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
 | n < 0 = []
 | otherwise = digitize (show n)

toRevDigits :: Integer -> [Integer]
toRevDigits n = reverse (toDigits n)

-- Exercise 3 -----------------------------------------
--  Ex: doubleEveryOther [4, 9, 5, 5] == [4, 18, 5, 10]
--  Ex: doubleEveryOther [0, 0] == [0, 0]

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y:xs) = x : 2*y : doubleEveryOther xs
doubleEveryOther [n] = [n]

-- Exercise 4 -----------------------------------------
--  Ex: sumDigits [10, 5, 18, 4] = 1 + 0 + 5 + 1 + 8 + 4 = 19

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs
sumDigits [n] = n


-- Exercise 5 -----------------------------------------
--  Example: luhn 5594589764218858 = True
--  Example: luhn 1234567898765432 = False


-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = mod (sumDigits (doubleEveryOther (toRevDigits n))) 10 == 0

-- Exercise 6 -----------------------------------------
--  Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
--  Solution: Move all but the last disc from a to c, using b as temporary storage.
--   Then, move the last disc from a to b.
--   Finally, move all but the last disc from c to b, using a as temporary storage.

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
