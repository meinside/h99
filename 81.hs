{--
https://wiki.haskell.org/99_questions/80_to_89

(**) Path from one node to another one

Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b.

Example in Haskell:

-- https://wiki.haskell.org/99_questions/Solutions/81
--}
--
-- (my solution, looks awkward, loops infinitely when there's no available path...)
paths :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths _ _ [] = []
paths from to xs
  | from == to = [[to]]
  | otherwise = map (from :) $ concat [paths f to xs | f <- nextStartPoints]
  where
    nextStartPoints = map snd $ filter (\(f, _) -> f == from) xs

--
-- (from the solution,)
paths2 :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths2 from to edges
  | from == to = [[to]]
  | otherwise =
    [ from : path
    | edge <- edges
    , fst edge == from
    , path <- paths2 (snd edge) to [e | e <- edges, e /= edge]
    ]
