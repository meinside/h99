{--
https://wiki.haskell.org/99_questions/11_to_20

(**) Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

Example in Haskell:

*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"

-- https://wiki.haskell.org/99_questions/Solutions/18
--}
--
-- (my solution)
slice :: [a] -> Int -> Int -> [a]
slice xs from to = drop (from - 1) (take to xs)

-- (more elegant way with list comprehension, 'zip' and 'list comprehension' are really cool...)
slice2 :: [a] -> Int -> Int -> [a]
slice2 xs start end = [x | (x, y) <- zip xs [1 .. end], y >= start]
