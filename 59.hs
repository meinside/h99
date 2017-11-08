{--
https://wiki.haskell.org/99_questions/54A_to_60

(**) Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Construct a list of all height-balanced binary trees with the given element and the given maximum height.

Example in Haskell:

*Main> take 4 $ hbalTree 'x' 3
[Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]

-- https://wiki.haskell.org/99_questions/Solutions/59
--}
--
data Tree a
  = Empty
  | Branch a
           (Tree a)
           (Tree a)
  deriving (Show, Eq)

--
-- (my solution, not efficient)
hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = []
hbalTree a n = filter (\t -> height t == n) (allTrees a n)

-- (altered from _55.hs)
allTrees :: a -> Int -> [Tree a]
allTrees _ 0 = [Empty]
allTrees a n =
  [ Branch a left right
  | d1 <- [0 .. (n - 1)]
  , d2 <- [0 .. (n - 1)]
  , left <- allTrees a d1
  , right <- allTrees a d2
  ]

height :: Tree a -> Int
height Empty = 0
height (Branch _ l r) = foldr max 0 heights
  where
    heights = [1 + height subtree | subtree <- [l, r]]

-- (from the solution, not fully understood...)
hbalTree2 :: a -> Int -> [Tree a]
hbalTree2 x 0 = [Empty]
hbalTree2 x 1 = [Branch x Empty Empty]
hbalTree2 x h =
  [ Branch x l r
  | (hl, hr) <- [(h - 2, h - 1), (h - 1, h - 1), (h - 1, h - 2)]
  , l <- hbalTree2 x hl
  , r <- hbalTree2 x hr
  ]
