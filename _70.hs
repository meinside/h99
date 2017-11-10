{--
https://wiki.haskell.org/99_questions/70B_to_73

(**) Tree construction from a node string.

We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever, during the tree traversal, the move is a backtrack to the previous level.

By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^

<https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p70.gif>

Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when the String is given. Make your predicate work in both directions.

Example in Haskell:

Tree> stringToTree "afg^^c^bd^e^^^"
Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]
 
Tree> treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])
"afg^^c^bd^e^^^"

-- https://wiki.haskell.org/99_questions/Solutions/70
--}
--
import Data.List

--
data Tree a =
  Node a
       [Tree a]
  deriving (Eq, Show)

--
-- (from the solution,)
-- XXX - not understood...
stringToTree :: String -> Tree Char
stringToTree (x:'^':"") = Node x []
stringToTree (x:xs) = Node x ys
  where
    z =
      map fst $
      filter ((==) 0 . snd) $
      zip [0 ..] $
      scanl (+) 0 $
      map
        (\x ->
           if x == '^'
             then -1
             else 1)
        xs
    ys = map (stringToTree . uncurry (sub xs)) $ zip (init z) (tail z)
    sub s a b = take (b - a) $ drop a s

treeToString :: Tree Char -> String
treeToString (Node x ts) =
  [x] ++ (concat $ intersperse "^" (map treeToString ts)) ++ "^"
