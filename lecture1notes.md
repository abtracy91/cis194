

    -- In code cells, you can write Haskell code.
    -- Press Shift-Enter or Ctrl-Enter to run and evaluate the code.
    -- Shift-Enter creates a new cell after running your code.
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    let fifth = fibs !! 5
    print fifth


    5



    x :: Int
    x = 3
    print x


    3



    y :: Int
    y = y + 1
    print y


    biggestInt, smallestInt :: Int
    biggestInt = maxBound
    smallestInt = minBound
    print biggestInt
    print smallestInt



    9223372036854775807



    -9223372036854775808



    d1, d2 :: Double
    d1 = 4.5387
    d2 = 6.2378e-4



    ex1 = 3+2
    print ex1



    5



    + 3 4


<span class='err-msg'>Parse error (line 1, column 1): parse error on input ‘+’</span>



    (+ 3 4)


<span class='err-msg'>Could not deduce (Num (a0 -> a)) arising from the ambiguity check for ‘it’<br/>from the context (Num (a1 -> a), Num a1, Num a) bound by the inferred type for ‘it’: (Num (a1 -> a), Num a1, Num a) => a -> a at <interactive>:1:1-7<br/>The type variable ‘a0’ is ambiguous<br/>When checking that ‘it’ has the inferred type ‘forall a a1. (Num (a1 -> a), Num a1, Num a) => a -> a’<br/>Probable cause: the inferred type is ambiguous</span>



    d :: Double
    d = 1.38455
    di = round d
    x = di + 1
    print x


    2



    1 / 2


    0.5



    i :: Int
    i = 1
    k :: Int
    k = 2
    div i k


    0



    sumtorial :: Integer -> Integer
    sumtorial 0 = 0
    sumtorial n = n + sumtorial (n - 1)



    hailstone :: Integer -> Integer
    hailstone n
     | n `mod` 2 == 0 = n `div` 2
     | otherwise = 3*n + 1
     
    x :: Integer
    x = 3
    hailstone x
    
    hailstone 3


    10



    10



    hailstone1 :: Integer -> Integer
    hailstone1 n
     | n `mod` 2 == 0 = n `div` 2
     | True = 3*n + 1
    
    hailstone1 3


 <div class="suggestion-name" style="clear:both;">Use otherwise</div>  <div class="suggestion-row" style="float: left;"> <div class="suggestion-warning">Found:</div>  <div class="highlight-code" id="haskell">hailstone1 :: Integer -> Integer hailstone1 n
  | n `mod` 2 == 0 = n `div` 2
  | True = 3 * n + 1</div> </div>  <div class="suggestion-row" style="float: left;"> <div class="suggestion-warning">Why Not:</div>  <div class="highlight-code" id="haskell">hailstone1 :: Integer -> Integer hailstone1 n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3 * n + 1</div> </div> 



    10



    sumPair :: (Int, Int) -> Int
    sumPair (x,y) = x + y
    
    sumPair (2,2)
    sumPair (2, 3)


    4



    5



    hello1 :: [Char]
    hello1 = ['h', 'e', 'l', 'l', 'o']
    
    hello2 :: String
    hello2 = "hello"
    
    helloSame = hello1 == hello2
    print helloSame


    hailstoneSeq :: Integer -> [Integer]
    hailstoneSeq 1 = [1]
    hailstoneSeq n = n : hailstoneSeq (hailstone n)
    
    hailstoneSeq 8



    [8,4,2,1]



    hailstone 2



    1



    sumEveryTwo :: [Integer] -> [Integer]
    sumEveryTwo [] = [] -- Do nothing to empty list
    sumEveryTwo (x:[]) = [x] -- Do nothing to 1 element
    sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs
    
    sumEveryTwo [1, 2, 1, 2, 1, 3, 1, 3, 5, 5]



    [3,3,4,4,10]



    -- Compute the length of a list on Integers
    intListLength :: [Integer] -> Integer
    intListLength [] = 0
    intListLength (x:xs) = 1 + intListLength xs
    
    intListLength [1,2,3,4,5,6]


    6



    hailstoneLen :: Integer -> Integer
    hailstoneLen n = intListLength (hailstoneSeq n) - 1
    
    hailstoneLen 12



    9



    
