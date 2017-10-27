{--
https://wiki.haskell.org/99_questions/11_to_20

(**) Drop every N'th element from a list.

Example in Haskell:

*Main> dropEvery "abcdefghik" 3
"abdeghk"

-- https://wiki.haskell.org/99_questions/Solutions/16
--}
--
import Data.List.Split -- for 'chunksOf'

--
-- (my solution)
dropEvery :: [a] -> Int -> [a]
--dropEvery xs n = concat (map (\x -> init x) (chunksOf n xs))
--dropEvery xs n = concat (map init (chunksOf n xs))
dropEvery xs n = concatMap init (chunksOf n xs)

--
-- (my solution without 'chunksOf')
dropEvery2 :: [a] -> Int -> [a]
dropEvery2 [] _ = []
dropEvery2 xs n = drop' xs n 1
  where
    drop' [] _ _ = []
    drop' (y:ys) num cnt
      | mod cnt num == 0 = drop' ys num (cnt + 1)
      | otherwise = y : drop' ys num (cnt + 1)
