{--
https://wiki.haskell.org/99_questions/21_to_28

Group the elements of a set into disjoint subsets.

Example in Haskell:

27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
(altogether 1260 solutions)
 
27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
(altogether 756 solutions)

-- https://wiki.haskell.org/99_questions/Solutions/27
--}
--
-- (from the solution,)
-- XXX - not understood...
combination :: Int -> [a] -> [([a], [a])]
combination 0 xs = [([], xs)]
combination _ [] = []
combination n (x:xs) = ts ++ ds
  where
    ts = [(x : ys, zs) | (ys, zs) <- combination (n - 1) xs]
    ds = [(ys, x : zs) | (ys, zs) <- combination n xs]

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs = [g : gs | (g, rs) <- combination n xs, gs <- group ns rs]
