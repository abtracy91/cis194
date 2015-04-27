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

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = undefined


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = undefined

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
