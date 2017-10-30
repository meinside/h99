{--
https://wiki.haskell.org/99_questions/11_to_20

(*) Remove the K'th element from a list.

Example in Haskell:

*Main> removeAt 2 "abcd"
('b',"acd")

-- https://wiki.haskell.org/99_questions/Solutions/20
--}
--
-- (my solution, using 'zip' and 'list comprehension')
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs =
  let pairs = zip xs [1 ..]
  in (xs !! (n - 1), [y | (y, index) <- pairs, index /= n])

-- (easier solution with 'take' and 'drop')
removeAt2 :: Int -> [a] -> (a, [a])
removeAt2 n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)
