{--
https://wiki.haskell.org/99_questions/54A_to_60

(**) Construct completely balanced binary trees

In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.

Example:

* cbal-tree(4,T).
T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
etc......No
Example in Haskell, whitespace and "comment diagrams" added for clarity and exposition:

*Main> cbalTree 4
[
-- permutation 1
--     x
--    / \
--   x   x
--        \
--         x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)),
 
-- permutation 2
--     x
--    / \
--   x   x
--      /
--     x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty),
 
-- permutation 3
--     x
--    / \
--   x   x
--    \
--     x
Branch 'x' (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)) 
           (Branch 'x' Empty Empty),
 
-- permutation 4
--     x
--    / \
--   x   x
--  /
-- x
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty) 
           (Branch 'x' Empty Empty)
]

-- https://wiki.haskell.org/99_questions/Solutions/55
--}
--
-- (from the solution,)
data Tree a
  = Empty
  | Branch a
           (Tree a)
           (Tree a)
  deriving (Show)

-- XXX - not fully understood...
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n =
  let (q, r) = (n - 1) `quotRem` 2 -- 'n - 1' will be devided into two branches ('1' = root branch)
  in [ Branch 'x' left right
     | i <- [q .. q + r] -- 'i' ranges from 'q' to 'q + r' (=> permutation)
     , left <- cbalTree i -- <= size of 'left'
     , right <- cbalTree (n - i - 1) -- <= and 'right' sum up to 'n - 1'
     ]
