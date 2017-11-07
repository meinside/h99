{--
https://wiki.haskell.org/99_questions/54A_to_60

(**) Generate-and-test paradigm

Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

Example in Haskell:

*Main> symCbalTrees 5
[Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]


-- https://wiki.haskell.org/99_questions/Solutions/58
--}
--
data Tree a
  = Empty
  | Branch a
           (Tree a)
           (Tree a)
  deriving (Show, Eq)

-- (from 55.hs,)
trees :: Int -> [Tree Char]
trees 0 = [Empty]
trees n =
  let (q, r) = (n - 1) `quotRem` 2
  in [ Branch 'x' left right
     | i <- [q .. q + r]
     , left <- trees i
     , right <- trees (n - 1 - i)
     ]

-- (from 57.hs,)
mirror :: (Eq a) => Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror l2 r1
mirror _ _ = False

symmetric :: (Eq a) => Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

-- (my solution, not efficient)
symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric $ trees n

-- (from the solution,)
symCbalTrees' :: Int -> [Tree Char]
symCbalTrees' n =
  if n `mod` 2 == 0
    then []
    else [Branch 'x' t (reverseTree t) | t <- trees (n `div` 2)] -- insert a reversed sub tree

reverseTree :: Tree a -> Tree a
reverseTree Empty = Empty
reverseTree (Branch x l r) = Branch x (reverseTree r) (reverseTree l)
