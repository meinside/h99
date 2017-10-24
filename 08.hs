{--
https://wiki.haskell.org/99_questions/1_to_10

(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example in Haskell:

> compress "aaaabccaadeeee"
"abcade"

-- https://wiki.haskell.org/99_questions/Solutions/8
--}
--
-- (my solution)
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress xs =
  foldr
    (\v acc ->
       if null acc
         then [v]
         else if v == head acc
                then acc
                else v : acc)
    []
    xs

-- dropWhile is cool!
compress2 :: (Eq a) => [a] -> [a]
compress2 [] = []
compress2 (x:xs) = x : compress2 (dropWhile (== x) xs)
