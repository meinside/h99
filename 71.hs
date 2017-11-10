{--
https://wiki.haskell.org/99_questions/70B_to_73

(*) Determine the internal path length of a tree.

We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. By this definition, tree5 has an internal path length of 9.

Example in Haskell:

Tree> ipl tree5
9
Tree> ipl tree4
2

-- https://wiki.haskell.org/99_questions/Solutions/71
--}
--
--
data Tree a =
  Node a
       [Tree a]
  deriving (Eq, Show)

--
-- (my solution)
ipl :: Tree a -> Int
ipl t = sum $ map snd $ walk 0 t -- sum up all internal path lengths
  where
    walk :: Int -> Tree a -> [(a, Int)] -- label each node with its interal path length
    walk n (Node a xs) =
      (a, n) : concat [walk (n + 1) node | node@(Node x _) <- xs]

--
-- (from the solution,)
ipl2 :: Tree a -> Int
ipl2 t = ipl2' 0 t
  where
    ipl2' :: Int -> Tree a -> Int
    ipl2' n (Node _ xs) = n + sum (map (ipl2' (n + 1)) xs)
