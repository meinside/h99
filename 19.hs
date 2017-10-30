{--
https://wiki.haskell.org/99_questions/11_to_20

(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).

Examples in Haskell:

*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
 
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"

-- https://wiki.haskell.org/99_questions/Solutions/19
--}
--
-- (my solution)
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n
  | n > 0 = rotate (tail xs ++ [head xs]) (n - 1)
  | n < 0 = rotate (last xs : init xs) (n + 1)
  | otherwise = xs

-- (my second solution)
rotate2 :: [a] -> Int -> [a]
--rotate2 xs n
--  | n > 0 = drop n xs ++ take n xs
--  | n < 0 =
--    let m = length xs + n
--    in drop m xs ++ take m xs
--  | otherwise = xs
rotate2 xs n =
  let m =
        if n >= 0
          then n
          else length xs + n
  in drop m xs ++ take m xs
