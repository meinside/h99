{--
https://wiki.haskell.org/99_questions/54A_to_60

(**) Symmetric binary trees

Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.

Example in Haskell:

*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
False
*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
True

-- https://wiki.haskell.org/99_questions/Solutions/56
--}
--
data Tree a
  = Empty
  | Branch a
           (Tree a)
           (Tree a)
  deriving (Show, Eq)

-- (my solution)
mirror :: (Eq a) => Tree a -> Tree a
mirror Empty = Empty
mirror (Branch a l r) = Branch a (mirror r) (mirror l)

symmetric :: (Eq a) => Tree a -> Bool
symmetric x = x == mirror x
