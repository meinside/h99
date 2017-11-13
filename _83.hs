{--
https://wiki.haskell.org/99_questions/80_to_89

(**) Construct all spanning trees

Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees of a given graph. With this predicate, find out how many spanning trees there are for the graph depicted to the left. The data of this example graph can be found in the file p83.dat. When you have a correct solution for the s_tree/2 predicate, use it to define two other useful predicates: is_tree(Graph) and is_connected(Graph). Both are five-minutes tasks!

Example in Haskell:

length $ spantree k4
16

-- https://wiki.haskell.org/99_questions/Solutions/83
--}
--
--
data Graph a =
  Graph [a]
        [(a, a)]
  deriving (Show, Eq)

--
-- (from the solution,)
-- XXX - ???
paths' :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths' a b xs
  | a == b = [[a]]
  | otherwise =
    concat
      [map (a :) $ paths' d b [x | x <- xs, x /= (c, d)] | (c, d) <- xs, c == a] ++
    concat
      [map (a :) $ paths' c b [x | x <- xs, x /= (c, d)] | (c, d) <- xs, d == a]

cycle' :: (Eq a) => a -> [(a, a)] -> [[a]]
cycle' a xs =
  [ a : path
  | e <- xs
  , fst e == a
  , path <- paths' (snd e) a [x | x <- xs, x /= e]
  ] ++
  [ a : path
  | e <- xs
  , snd e == a
  , path <- paths' (fst e) a [x | x <- xs, x /= e]
  ]

spantree :: (Eq a) => Graph a -> [Graph a]
spantree (Graph xs ys) =
  filter connected $ filter (not . cycles) $ filter nodes alltrees
  where
    alltrees = [Graph (ns edges) edges | edges <- foldr acc [[]] ys]
    acc e es = es ++ map (e :) es
    ns e =
      foldr
        (\x xs ->
           if x `elem` xs
             then xs
             else x : xs)
        [] $
      concat $ map (\(a, b) -> [a, b]) e
    nodes (Graph xs' ys') = length xs == length xs'
    cycles (Graph xs' ys') = any ((/=) 0 . length . flip cycle' ys') xs'
    connected (Graph (x':xs') ys') =
      not $ any null [paths' x' y' ys' | y' <- xs']
