{--
https://wiki.haskell.org/99_questions/61_to_69

Collect the internal nodes of a binary tree in a list

An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list.

Example in Haskell:

Prelude> internals tree4
Prelude> [1,2]

-- https://wiki.haskell.org/99_questions/Solutions/62
--}
--
data Tree a
  = Empty
  | Branch a
           (Tree a)
           (Tree a)
  deriving (Show, Eq)

--
-- (my solution)
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch a left right) = a : internals left ++ internals right
