{--
https://wiki.haskell.org/99_questions/70B_to_73

(*) Count the nodes of a multiway tree.

Example in Haskell:

Tree> nnodes tree2
2

-- https://wiki.haskell.org/99_questions/Solutions/70C
--}
--
data Tree a =
  Node a
       [Tree a]
  deriving (Eq, Show)

--
-- (my solution)
nnodes :: Tree a -> Int
--nnodes (Node _ []) = 1
nnodes (Node _ xs) = 1 + sum (map nnodes xs)
