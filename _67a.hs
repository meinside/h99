{--
https://wiki.haskell.org/99_questions/61_to_69

A string representation of binary trees

Somebody represents binary trees as strings of the following type:

a(b(d,e),c(,f(g,)))
a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.

Example in Haskell:

Main> stringToTree "x(y,a(,b))" >>= print
Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
Main> let t = cbtFromList ['a'..'z'] in stringToTree (treeToString t) >>= print . (== t)
True

-- https://wiki.haskell.org/99_questions/Solutions/67A
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
treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) =
  x : '(' : treeToString l ++ "," ++ treeToString r ++ ")"

stringToTree :: (Monad m) => String -> m (Tree Char)
stringToTree "" = return Empty
stringToTree [x] = return $ Branch x Empty Empty
stringToTree str = tfs str >>= \("", t) -> return t
  where
    tfs a@(x:xs)
      | x == ',' || x == ')' = return (a, Empty)
    tfs (x:y:xs)
      | y == ',' || y == ')' = return (y : xs, Branch x Empty Empty)
      | y == '(' = do
        (',':xs', l) <- tfs xs
        (')':xs'', r) <- tfs xs'
        return (xs'', Branch x l r)
    tfs _ = fail "bad parse"
