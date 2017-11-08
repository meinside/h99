{--
https://wiki.haskell.org/99_questions/61_to_69

Collect the leaves of a binary tree in a list

A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.

Example:

% leaves(T,S) :- S is the list of all leaves of the binary tree T
Example in Haskell:

> leaves tree4
[4,2]

-- https://wiki.haskell.org/99_questions/Solutions/61A
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
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch _ left right) = leaves left ++ leaves right
