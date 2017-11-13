{--
https://wiki.haskell.org/99_questions/80_to_89

(*) Cycle from a given node

Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G. The predicate should return all cycles via backtracking.

Example in Haskell:

graph> cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[[2,3,4,2]]
graph> cycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[]

-- https://wiki.haskell.org/99_questions/Solutions/82
--}
--
import Data.List -- for 'partition'

--
-- (from the solution,)
cycle' :: (Eq a) => a -> [(a, a)] -> [[a]]
cycle' n g = search [[n]] []
  where
    search [] result = result
    search cur result = search (go active) (arrive ++ result)
      where
        split = partition end cur
        end s = (last s == n) && (length s /= 1)
        active = snd split
        arrive = fst split
        go ls =
          [ x ++ [snd y]
          | x <- ls
          , y <- g
          , last x == fst y
          , not (snd y `elem` tail x)
          ]
