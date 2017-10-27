{--
https://wiki.haskell.org/99_questions/11_to_20

(*) Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates.

Example in Haskell:

*Main> split "abcdefghik" 3
("abc", "defghik")

-- https://wiki.haskell.org/99_questions/Solutions/17
--}
--
-- (my solution)
split :: [a] -> Int -> ([a], [a])
--split xs n = balance ([], xs) n
split xs = balance ([], xs)
  where
    balance :: ([a], [a]) -> Int -> ([a], [a])
    balance (ys, []) _ = (ys, [])
    balance (ys, w@(z:zs)) n
      | length ys < n = balance (ys ++ [z], zs) n
      | otherwise = (ys, w)

-- (better solution)
-- 'let/in' is useful...
split2 :: [a] -> Int -> ([a], [a])
split2 [] _ = ([], [])
split2 (x:xs) n
  | n > 0 =
    let (l, r) = split2 xs (n - 1)
    in (x : l, r)
  | otherwise = ([], x : xs)

-- (much better...)
split3 :: [a] -> Int -> ([a], [a])
split3 [] _ = ([], [])
split3 (x:xs) n
  | n > 0 =
    let (l, r) = split2 xs (n - 1)
    in (x : l, r)
split3 xs _ = ([], xs)

-- (shorter solution with predefined predicates,)
split' :: [a] -> Int -> ([a], [a])
split' xs n = (take n xs, drop n xs)
