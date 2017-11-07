{--
https://wiki.haskell.org/99_questions/54A_to_60

(**) Binary search trees (dictionaries)

Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.

Example:

* construct([3,2,5,7,1],T).
T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
Then use this predicate to test the solution of the problem P56.

Example in Haskell:

*Main> construct [3, 2, 5, 7, 1]
Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
*Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
True
*Main> symmetric . construct $ [3, 2, 5, 7, 1]
True

-- https://wiki.haskell.org/99_questions/Solutions/57
--}
--
-- (from 56.hs,)
data Tree a
  = Empty
  | Branch a
           (Tree a)
           (Tree a)
  deriving (Show, Eq)

-- (modified from 56.hs,)
mirror :: (Eq a) => Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror l2 r1 -- doesn't care about the node value
mirror _ _ = False

symmetric :: (Eq a) => Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

-- (from the solution,)
add :: (Ord a) => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x t@(Branch y l r)
  | x > y = Branch y l (add x r)
  | x < y = Branch y (add x l) r
  | otherwise = t

construct :: (Ord a) => [a] -> Tree a
construct xs = foldl (flip add) Empty xs
