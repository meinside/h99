{--
https://wiki.haskell.org/99_questions/70B_to_73

(*) Construct the bottom-up order sequence of the tree nodes.

Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the nodes of the multiway tree Tree.

Example in Haskell:

Tree> bottom_up tree5
"gfcdeba"

-- https://wiki.haskell.org/99_questions/Solutions/72
--}
--
--
data Tree a =
  Node a
       [Tree a]
  deriving (Eq, Show)

--
-- (from the solution)
-- XXX - I don't even understand the question...
bottomUp :: Tree a -> [a]
bottomUp (Node a xs) = concatMap bottomUp xs ++ [a]

bottomUp2 :: Tree a -> [a]
bottomUp2 t = bottomUpAux t []
  where
    bottomUpAux :: Tree a -> [a] -> [a]
    bottomUpAux (Node x ts) xs = foldr bottomUpAux (x : xs) ts
