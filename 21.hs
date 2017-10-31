{--
https://wiki.haskell.org/99_questions/21_to_28

Insert an element at a given position into a list.

Example in Haskell:

P21> insertAt 'X' "abcd" 2
"aXbcd"

-- https://wiki.haskell.org/99_questions/Solutions/21
--}
--
-- (my solution using 'take' and 'drop')
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys n = take (n - 1) ys ++ [x] ++ drop (n - 1) ys

-- (my another solution)
insertAt2 :: a -> [a] -> Int -> [a]
insertAt2 x ys 1 = x : ys
insertAt2 x [] _ = [x]
insertAt2 x (y:ys) n = y : insertAt2 x ys (n - 1)
