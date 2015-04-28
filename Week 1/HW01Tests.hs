-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToDigits :: (Integer, [Integer]) -> Bool
testToDigits (n, d) = toDigits n == d

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, d) = toRevDigits n == d

ex2Tests :: [Test]
ex2Tests = [ Test "toDigits test" testToDigits
             [(1234, [1,2,3,4]), (0, []), ((-17), [])]
           , Test "toRevDigits test" testToRevDigits
             [(1234, [4,3,2,1]), (0, []), ((-17), [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([4,9,5,5], [4,18,5,10]), ([0,0], [0,0])]]

-- Exercise 4 -----------------------------------------
--  Ex: sumDigits [10, 5, 18, 4] = 1 + 0 + 5 + 1 + 8 + 4 = 19

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (n, d) = sumDigits n == d

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
             [([10, 5, 18, 4], 19)]]

-- Exercise 5 -----------------------------------------
--  Example: luhn 5594589764218858 = True
--  Example: luhn 1234567898765432 = False

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, d) = luhn n == d

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [(5594589764218858, True), (1234567898765432, False)]]

-- Exercise 6 -----------------------------------------
--  Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, p, q, r, d) = hanoi n p q r == d

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [(2, "a" :: Peg, "b" :: Peg, "c" :: Peg, [("a","c") :: Move, ("a","b") :: Move, ("c","b") :: Move])]]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
