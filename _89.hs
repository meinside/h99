{--
https://wiki.haskell.org/99_questions/80_to_89

(**) Bipartite graphs

Write a predicate that finds out whether a given graph is bipartite.

Example in Haskell:

bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])
True
bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])
False

-- https://wiki.haskell.org/99_questions/Solutions/89
--}
--
-- (from the solution,)
-- XXX - ??? 
import Data.List

type Node = Int

type Edge = (Node, Node)

type Graph = ([Node], [Edge])

dfsbipartite :: Graph -> [(Node, Int)] -> [Node] -> [Node] -> Bool
dfsbipartite ([], _) _ _ _ = True
dfsbipartite (_, _) [] _ _ = True
dfsbipartite (v, e) ((nv, 0):stack) odd even
  | [x | x <- v, x == nv] == [] = dfsbipartite (v, e) stack odd even
  | [] == intersect adjacent even =
    dfsbipartite (newv, e) ([(x, 1) | x <- adjacent] ++ stack) odd (nv : even)
  | otherwise = False
  where
    adjacent = [x | (x, y) <- e, y == nv] ++ [x | (y, x) <- e, y == nv]
    newv = [x | x <- v, x /= nv]
dfsbipartite (v, e) ((nv, 1):stack) odd even
  | [x | x <- v, x == nv] == [] = dfsbipartite (v, e) stack odd even
  | [] == intersect adjacent odd =
    dfsbipartite (newv, e) ([(x, 0) | x <- adjacent] ++ stack) (nv : odd) even
  | otherwise = False
  where
    adjacent = [x | (x, y) <- e, y == nv] ++ [x | (y, x) <- e, y == nv]
    newv = [x | x <- v, x /= nv]

bipartite :: Graph -> Bool
bipartite ([], _) = True
bipartite (top:v, e) = dfsbipartite (top : v, e) [(top, 0)] [] []
