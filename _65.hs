{--
https://wiki.haskell.org/99_questions/61_to_69

An alternative layout method is depicted in the illustration below:

<https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p65.gif>

Find out the rules and write the corresponding function. Hint: On a given level, the horizontal distance between neighboring nodes is constant.

Use the same conventions as in problem P64 and test your function in an appropriate way.

Here is the example tree from the above illustration:

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )
Example in Haskell:

> layout tree65
Branch ('n',(15,1)) (Branch ('k',(7,2)) (Branch ('c',(3,3)) ...

-- https://wiki.haskell.org/99_questions/Solutions/65
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

depth :: Tree a -> Int
depth Empty = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

leftDepth :: Tree a -> Int
leftDepth Empty = 0
leftDepth (Branch _ l _) = leftDepth l + 1

layout :: Tree a -> Tree (a, Pos)
layout t = layoutAux xl 1 sepl t
  where
    d = depth t
    ld = leftDepth t
    xl = 2 ^ (d - 1) - 2 ^ (d - ld) + 1
    sepl = 2 ^ (d - 2)
    layoutAux :: Int -> Int -> Int -> Tree a -> Tree (a, Pos)
    layoutAux _ _ _ Empty = Empty
    layoutAux x y sep (Branch a l r) =
      Branch
        (a, (x, y))
        (layoutAux (x - sep) (y + 1) (sep `div` 2) l)
        (layoutAux (x + sep) (y + 1) (sep `div` 2) r)
