{--
https://wiki.haskell.org/99_questions/46_to_50

(**) Truth tables for logical expressions (3).

Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.

Example in Haskell:

> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- infixl 3 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False True
False True  True  True
False True  False True
False False True  True
False False False True
 
-- infixl 7 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False False
False True  True  False
False True  False False
False False True  False
False False False False

-- https://wiki.haskell.org/99_questions/Solutions/48
--}
--
import Control.Monad (replicateM)

-- (from the solution)
--
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

infixl 4 `or'`

infixl 6 `and'`

infixl 3 `equ'`

--infixl 7 `equ'`
tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = [args ++ [f args] | args <- replicateM n [True, False]] -- replicateM !!!

printTable :: Int -> ([Bool] -> Bool) -> IO ()
printTable n f = putStrLn $ unlines $ map show $ tablen n f
