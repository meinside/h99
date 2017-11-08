{--
https://wiki.haskell.org/99_questions/61_to_69

Collect the nodes at a given level in a list

A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.

Example in Haskell:

Prelude> atLevel tree4 2
Prelude> [2,2]


-- https://wiki.haskell.org/99_questions/Solutions/62B
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
atLevel :: Tree a -> Int -> [a]
atLevel _ 0 = []
atLevel Empty _ = []
atLevel (Branch a _ _) 1 = [a]
atLevel (Branch _ left right) n = atLevel left (n - 1) ++ atLevel right (n - 1)

--
-- (from the solution,)
atLevel2 :: Tree a -> Int -> [a]
atLevel2 Empty _ = []
atLevel2 (Branch v l r) n
  | n == 1 = [v]
  | n > 1 = atLevel2 l (n - 1) ++ atLevel2 r (n - 1)
  | otherwise = []
