{--
https://wiki.haskell.org/99_questions/61_to_69

Yet another layout strategy is shown in the illustration below:

<https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p66.gif>

The method yields a very compact layout while maintaining a certain symmetry in every node. Find out the rules and write the corresponding Prolog predicate. Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together two subtrees to construct the combined binary tree?

Use the same conventions as in problem P64 and P65 and test your predicate in an appropriate way. Note: This is a difficult problem. Don't give up too early!

Which layout do you like most?

Example in Haskell:

> layout tree65
Branch ('n',(5,1)) (Branch ('k',(3,2)) (Branch ('c',(2,3)) ...

-- https://wiki.haskell.org/99_questions/Solutions/66
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
type Pos = (Int, Int)

layout :: Tree a -> Tree (a, Pos)
layout t = t'
  where
    (l, t', r) = layoutAux x1 1 t
    x1 = maximum l + 1
    layoutAux :: Int -> Int -> Tree a -> ([Int], Tree (a, Pos), [Int])
    layoutAux x y Empty = ([], Empty, [])
    layoutAux x y (Branch a l r) = (ll', Branch (a, (x, y)) l' r', rr')
      where
        (ll, l', lr) = layoutAux (x - sep) (y + 1) l
        (rl, r', rr) = layoutAux (x + sep) (y + 1) r
        sep = maximum (0 : zipWith (+) lr rl) `div` 2 + 1
        ll' = 0 : overlay (map (+ sep) ll) (map (subtract sep) rl)
        rr' = 0 : overlay (map (+ sep) rr) (map (subtract sep) lr)

-- overlay xs ys = xs padded out to at least the length of ys
-- using any extra elements of ys
overlay :: [a] -> [a] -> [a]
overlay [] ys = ys
overlay xs [] = xs
overlay (x:xs) (_:ys) = x : overlay xs ys
