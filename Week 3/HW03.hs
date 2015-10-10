module HW03 where

-- Homework 3
-- Used https://gist.github.com/adolfopa/2df36cc66dc7ecd2985e
--  & #haskell-beginners for extensive help

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
-- extend: existing State, String -> Int pair to add to State, Result: new state
-- ex: extend State("a", 1) ("b", 2) => State("a",1, "b",2)
extend st str1 val str2 = if str1 == str2 then val else st str2

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

evalE :: State -> Expression -> Int
evalE s (Var v) = s v -- Add var to state
evalE s (Val n) = n -- Val n is just a number
evalE s (Op lhs op rhs) =
    case op of
        Plus -> x + y
        Minus -> x - y
        Times -> x * y
        Divide -> x `div` y
        Gt -> boolToInt $ x > y
        Ge -> boolToInt $ x >= y
        Lt -> boolToInt $ x < y
        Le -> boolToInt $ x <= y
        Eql -> boolToInt $ x == y
    where x = evalE s lhs
          y = evalE s rhs

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

-- Example: desugar (Incr "A") == DAssign "A" (Op (var "A") Plus (Val 1))
desugar :: Statement -> DietStatement
desugar (Assign v e) = DAssign v e
desugar (Incr v) = DAssign v (Op (Var v) Plus (Val 1))
desugar (If e s1 s2) = DIf e (desugar s1) (desugar s2)
desugar (While e s) = DWhile e (desugar s)
desugar (For i e th el) = DSequence (desugar i) (DWhile e (DSequence (desugar el) (desugar th)))
desugar (Sequence x y) = DSequence (desugar x) (desugar y)
desugar Skip = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign v e) = extend s v (evalE s e)
evalSimple s (DIf e x y) = if (evalE s e) == 1 then evalSimple s x else evalSimple s y
evalSimple s w@(DWhile e x) = if (evalE s e) == 1 then evalSimple (evalSimple s x) w else s
evalSimple s (DSequence x y) = evalSimple (evalSimple s x) y
evalSimple s DSkip = s

run :: State -> Statement -> State
run state statement = evalSimple state $ desugar statement

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
