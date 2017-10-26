{--
https://wiki.haskell.org/99_questions/11_to_20

(**) Replicate the elements of a list a given number of times.

Example in Haskell:

> repli "abc" 3
"aaabbbccc"

-- https://wiki.haskell.org/99_questions/Solutions/15
--}
--
-- (my solution)
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

--
-- (my solution, without 'replicate')
repli2 :: [a] -> Int -> [a]
repli2 [] _ = []
repli2 (x:xs) n = ntimes x n ++ repli2 xs n
  where
    ntimes :: a -> Int -> [a]
    ntimes _ 0 = []
    ntimes a m = a : ntimes a (m - 1)

--
-- (monad!)
repli3 :: [a] -> Int -> [a]
repli3 xs n = xs >>= replicate n
