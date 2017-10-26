{--
https://wiki.haskell.org/99_questions/11_to_20

(*) Duplicate the elements of a list.

Example in Haskell:

> dupli [1, 2, 3]
[1,1,2,2,3,3]

-- https://wiki.haskell.org/99_questions/Solutions/14
--}
--
-- (my solution)
dupli :: [a] -> [a]
dupli = foldr f []
  where
    f x acc = x : x : acc

--
-- (this is a better solution...)
dupli2 :: [a] -> [a]
dupli2 [] = []
dupli2 (x:xs) = x : x : dupli2 xs

--
-- (this is much better!)
dupli3 :: [a] -> [a]
dupli3 xs = concat [[x, x] | x <- xs]
