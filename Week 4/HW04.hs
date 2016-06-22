module HW04 where

-- Homework 4


-- Exercise 1 -----------------------------------------
-- 

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' =  product . map (\x -> x - 2) . filter (\x -> even x)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


-- This solution was copied from:
--   https://github.com/BerndSchwarzenbacher/cis194-solutions/blob/master/04-higher-order/hw04.hs
-- It works.

fun2' :: Integer -> Integer
fun2' = sum
  . filter even
  . takeWhile (/= 1)
  . iterate (\n -> if even n then n `div` 2 else 3*n+1)


-- Exercise 2 -----------------------------------------
--   DONT UNDERSTAND

-- Write a function, foldTree, which generates a balanced binary tree from
--   list of values using foldr.
-- foldTree :: [a] -> Tree a
-- foldTree = ...

{- Example:
foldTree "ABCDEFGHIJ" == 
  Node 3
    (Node 2
      (Node 0 Leaf 'F' Leaf)
      'I'
      (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
    'J'
    (Node 2
      (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
      'H'
      (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
-}

data Tree a = Leaf
			      | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- This Solution was copied from:
--   https://github.com/BerndSchwarzenbacher/cis194-solutions/blob/master/04-higher-order/hw04.hs
--   Not sure if it is working

foldTree :: Eq a => [a] -> Tree a
foldTree xs = foldr (balancedInsert start) Leaf xs
  where start = floor (logBase 2 $ fromIntegral(length xs)::Double)

balancedInsert :: Int -> a -> Tree a -> Tree a
balancedInsert _ _ _ = Leaf

{-




-}

-- Exercise 3 -----------------------------------------

xor :: [Bool] -> Bool






























