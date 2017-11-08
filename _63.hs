{--
https://wiki.haskell.org/99_questions/61_to_69

Construct a complete binary tree

A complete binary tree with height H is defined as follows:

The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. For every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. This fact can be used to elegantly construct a complete binary tree structure.

Write a predicate complete_binary_tree/2.

Example in Haskell:

Main> completeBinaryTree 4
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
 
Main> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
True

-- https://wiki.haskell.org/99_questions/Solutions/63
--}
--
import Data.List

--
data Tree a
  = Empty
  | Branch a
           (Tree a)
           (Tree a)
  deriving (Show, Eq)

--
-- (from the solution,)
filled :: Tree a -> [[Bool]]
filled Empty = repeat [False]
filled (Branch _ l r) = [True] : zipWith (++) (filled l) (filled r) -- 'zilWith'!!!

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = generateTree 1
  where
    generateTree x
      | x > n = Empty
      | otherwise = Branch 'x' (generateTree (2 * x)) (generateTree (2 * x + 1))

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree t = and $ lastProper : zipWith (==) lengths powers
  where
    levels = takeWhile or $ filled t
    lengths = map (length . filter id) $ init levels
    powers = iterate (2 *) 1 -- 'iterate'!!!
    lastFilled = map head $ group $ last levels
    lastProper = head lastFilled && length lastFilled < 3
