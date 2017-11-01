{--
https://wiki.haskell.org/99_questions/21_to_28

(**) Generate the combinations of K distinct objects chosen from the N elements of a list

In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example in Haskell:

> combinations 3 "abcdef"
["abc","abd","abe",...]

-- https://wiki.haskell.org/99_questions/Solutions/26
--}
--
-- (my solution - wrong: doesn't filter out duplicated combinations, like '[[a, b], [b, a]]')
combinations :: Eq a => Int -> [a] -> [[a]]
combinations 1 xs = [[x] | x <- xs]
combinations n xs =
  [y ++ [x] | y <- combinations (n - 1) xs, x <- xs, x `notElem` y]

-- (from the solution...)
-- ex: combi 2 [1, 2, 3, 4]
-- = [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]
-- <= map (1:) [2, 3, 4] ++ map (2:) [3, 4] ++ map (3:) [4]
combinations2 :: Eq a => Int -> [a] -> [[a]]
combinations2 1 xs = map (: []) xs
combinations2 _ [] = []
combinations2 n (x:xs) =
  map (x :) (combinations2 (n - 1) xs) ++ combinations2 n xs
