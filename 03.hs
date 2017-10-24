{--
https://wiki.haskell.org/99_questions/1_to_10

(*) Find the K'th element of a list. The first element in the list is number 1.

Example:

* (element-at '(a b c d e) 3)
c
Example in Haskell:

Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'

-- https://wiki.haskell.org/99_questions/Solutions/3
--}
--
-- (my solution)
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)
elementAt [] _ = error "given array is empty"
