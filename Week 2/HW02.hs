{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
--  Ex: exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red] == 0
--  Ex: exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] == 2

comparePeg :: Peg -> Peg -> Int
comparePeg x y
 | x == y = 1
 | otherwise = 0

exactMatches :: Code -> Code -> Int
exactMatches [] (_) = 0
exactMatches (_) [] = 0
exactMatches (x:xs) (y:ys) = comparePeg x y + exactMatches xs ys





-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
--  Ex: countColors [Red, Blue, Yellow, Purple] == [1, 0, 1, 1, 0, 1]
--  Ex: countColors [Green, Blue, Green, Orange] == [0, 2, 1, 0, 1, 0]

sumArray :: [Int] -> [Int] -> [Int]
sumArray [] (_) = []
sumArray (_) [] = []
sumArray (x:xs) (y:ys) = (x + y) : sumArray xs ys

countColors' :: [Int] -> Code -> [Int]
countColors' [] (y) = countColors' [0, 0, 0, 0, 0, 0] (y)
countColors' x [] = x
countColors' x (y:ys)
 | y == Red = countColors' (sumArray x [1,0,0,0,0,0]) ys
 | y == Green = countColors' (sumArray x [0,1,0,0,0,0]) ys
 | y == Blue = countColors' (sumArray x [0,0,1,0,0,0]) ys
 | y == Yellow = countColors' (sumArray x [0,0,0,1,0,0]) ys
 | y == Orange = countColors' (sumArray x [0,0,0,0,1,0]) ys
 | y == Purple = countColors' (sumArray x [0,0,0,0,0,1]) ys
 | otherwise = countColors' x ys


countColors :: Code -> [Int]
countColors x = countColors' [] x

-- Count number of matches between the actual code and the guess
--  Example: matches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] == 3

matches' :: Int -> [Int] -> [Int] -> Int
matches' n [] (_) = n
matches' n (_) [] = n
matches' n (a:as) (b:bs)
 | (a > 0) && (b > 0) = matches' (n+1) ((a-1):as) ((b-1):bs)
 | otherwise = matches' n as bs

matches :: Code -> Code -> Int
matches x y = matches' 0 (countColors x) (countColors y)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
--  Example: getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] == 
--	  Move [Red, Orange, Orange, Blue] 1 2
getMove :: Code -> Code -> Move
getMove x y = Move y (exactMatches x y) ((matches x y) - 1)

-- Exercise 4 -----------------------------------------
--  Consistency: A Code is consistent with a Move if the Guess has the same number of exact and non-exact matches as the code.
--  Example: isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Yellow, Purple] == True
--  Example: isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Red, Purple] == False

get1Int :: Move -> Int
get1Int (Move _ int1 _) = int1

get2Int :: Move -> Int
get2Int (Move _ _ int2) = int2


isConsistent :: Move -> Code -> Bool
isConsistent (Move mc em m) c = get1Int (getMove mc c) == em && get2Int (getMove mc c) == m

-- Exercise 5 -----------------------------------------
-- Takes a move(?) and a list of codes.  Returns a list of codes consistent with the move.

filterCodes :: Move -> [Code] -> [Code]
filterCodes _ [] = []
filterCodes move (x:xs)
 | isConsistent move x = x : filterCodes move xs
 | otherwise = filterCodes move xs

-- Exercise 6 -----------------------------------------

allPegs :: [Peg]
allPegs = [Red,Green,Blue,Yellow,Orange,Purple]

initCodes :: [Code]
initCodes = [[Red],[Green],[Blue],[Yellow],[Orange],[Purple]]

addPegCode :: Peg -> Code -> Code
-- Adds a Peg to the beginning of a Code
addPegCode p [] = [p]
addPegCode p c = p : c

addPegsCode :: [Peg] -> Code -> [Code]
-- Adds each peg in a list of pegs to the beginning of a Code
addPegsCode [] code = []
addPegsCode pegs [] = [pegs]
addPegsCode (x:xs) code = (addPegCode x code) : (addPegsCode xs code)

merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Takes a list of Codes of length n-1, and returns a list of Codes of length n
-- Input: all possible codes, length n-1; Output: all possible codes, length n;
-- Solution: For each possible color, c, output += (Code c : input)
allCodesHelper :: [Code] -> [Code]
allCodesHelper [] = []
allCodesHelper (x:xs) = merge (addPegsCode allPegs x) (allCodesHelper xs)

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = initCodes
allCodes n = allCodesHelper (allCodes (n-1))

-- Exercise 7 -----------------------------------------
-- I am stuck on this exercise... going to continue with the course and come back to it later.

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined










