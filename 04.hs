{--
https://wiki.haskell.org/99_questions/1_to_10

(*) Find the number of elements of a list.

Example in Haskell:

Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13

-- https://wiki.haskell.org/99_questions/Solutions/4
--}
--
-- (my solution)
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
