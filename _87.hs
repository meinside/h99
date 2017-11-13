{--
https://wiki.haskell.org/99_questions/80_to_89

(**) Depth-first order graph traversal (alternative solution)

Write a predicate that generates a depth-first order graph traversal sequence. The starting point should be specified, and the output should be a list of nodes that are reachable from this starting point (in depth-first order).

Example in Haskell:

depthfirst ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]) 1
[1,2,3,4,5]

-- https://wiki.haskell.org/99_questions/Solutions/87
--}
--
-- (from the solution,)
-- XXX - ??? 
type Node = Int

type Edge = (Node, Node)

type Graph = ([Node], [Edge])

depthfirst :: Graph -> Node -> [Node]
depthfirst (v, e) n
  | [x | x <- v, x == n] == [] = []
  | otherwise = dfrecursive (v, e) [n]

dfrecursive :: Graph -> [Node] -> [Node]
dfrecursive ([], _) _ = []
dfrecursive (_, _) [] = []
dfrecursive (v, e) (top:stack)
  | [x | x <- v, x == top] == [] = dfrecursive (newv, e) stack
  | otherwise = top : dfrecursive (newv, e) (adjacent ++ stack)
  where
    adjacent = [x | (x, y) <- e, y == top] ++ [x | (y, x) <- e, y == top]
    newv = [x | x <- v, x /= top]
