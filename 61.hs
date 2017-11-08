{--
https://wiki.haskell.org/99_questions/61_to_69

Count the leaves of a binary tree

A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.

Example in Haskell:

> countLeaves tree4
2

-- https://wiki.haskell.org/99_questions/Solutions/61
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
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ left right) = countLeaves left + countLeaves right
