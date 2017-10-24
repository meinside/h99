{--
https://wiki.haskell.org/99_questions/1_to_10

(*) Reverse a list.

Example in Haskell:

Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1]

-- https://wiki.haskell.org/99_questions/Solutions/5
--}
--
-- (my solution)
myReverse :: [a] -> [a]
--myReverse [] = []
--myReverse (x:xs) = myReverse xs ++ [x]
--
-- (cool!)
myReverse xs = foldl (flip (:)) [] xs
