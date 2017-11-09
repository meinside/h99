{--
https://wiki.haskell.org/99_questions/61_to_69

Dotstring representation of binary trees.

We consider again binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67. Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted where an empty subtree (nil) is encountered during the tree traversal. For example, the tree shown in problem P67 is represented as 'abd..e..c.fg...'. First, try to establish a syntax (BNF or syntax diagrams) and then write a predicate tree_dotstring/2 which does the conversion in both directions. Use difference lists.

Example in Haskell:

> fst (ds2tree example)
Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
 
> tree2ds (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty))
"xy..z0..."

-- https://wiki.haskell.org/99_questions/Solutions/69
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
-- XXX - not understood... trees are so hard!!!
ds2tree :: String -> (Tree Char, String)
ds2tree [] = (Empty, "")
ds2tree ('.':xs) = (Empty, xs)
ds2tree (x:xs) = (Branch x left right, rest2)
  where
    (left, rest) = ds2tree xs
    (right, rest2) = ds2tree rest

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch x l r) = x : (tree2ds l ++ tree2ds r)
