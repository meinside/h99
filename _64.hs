{--
https://wiki.haskell.org/99_questions/61_to_69

Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration below:

<https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p64.gif>

In this layout strategy, the position of a node v is obtained by the following two rules:

x(v) is equal to the position of the node v in the inorder sequence
y(v) is equal to the depth of the node v in the tree
Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or the rectangle bounding the drawn tree.

Here is the example tree from the above illustration:

tree64 =
  Branch
    'n'
    (Branch
       'k'
       (Branch
          'c'
          (Branch 'a' Empty Empty)
          (Branch 'h' (Branch 'g' (Branch 'e' Empty Empty) Empty) Empty))
       (Branch 'm' Empty Empty))
    (Branch
       'u'
       (Branch 'p' Empty (Branch 's' (Branch 'q' Empty Empty) Empty))
       Empty)

Example in Haskell:

> layout tree64
Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...

-- https://wiki.haskell.org/99_questions/Solutions/64
--}
--
data Tree a
  = Empty
  | Branch a
           (Tree a)
           (Tree a)
  deriving (Show, Eq)

--
-- (from the solution,)
-- XXX - not understood...
type Pos = (Int, Int)

layout :: Tree a -> Tree (a, Pos)
layout t = fst (layoutAux 1 1 t)
  where
    layoutAux :: Int -> Int -> Tree a -> (Tree (a, Pos), Int)
    layoutAux x _ Empty = (Empty, x)
    layoutAux x y (Branch a l r) = (Branch (a, (x', y)) l' r', x'')
      where
        (l', x') = layoutAux x (y + 1) l
        (r', x'') = layoutAux (x' + 1) (y + 1) r
