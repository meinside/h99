{--
https://wiki.haskell.org/99_questions/46_to_50

(**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.

A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).

Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

Example in Haskell:

> table (\a b -> (and' a (or' a b)))
True True True
True False True
False True False
False False False

-- https://wiki.haskell.org/99_questions/Solutions/46
--}
--
-- (my solution)
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table f = [(a, b, f a b) | a <- [True, False], b <- [True, False]]

printTable :: (Bool -> Bool -> Bool) -> IO ()
printTable f = putStrLn $ unlines $ map show $ table f
