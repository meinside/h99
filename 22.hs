{--
https://wiki.haskell.org/99_questions/21_to_28

Create a list containing all integers within a given range.

Example in Haskell:

Prelude> range 4 9
[4,5,6,7,8,9]

-- https://wiki.haskell.org/99_questions/Solutions/22
--}
--
-- (my solution)
range :: Int -> Int -> [Int]
range from to = [from .. to]

-- (my another solution)
range2 :: Int -> Int -> [Int]
range2 from to = drop (from - 1) $ take to [1,2 ..]
