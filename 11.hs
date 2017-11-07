{--
https://wiki.haskell.org/99_questions/11_to_20

(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example in Haskell:

P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

-- https://wiki.haskell.org/99_questions/Solutions/11
--}
--
import Data.List -- for 'group' function

--
data Element a
  = Multiple Int
             a
  | Single a
  deriving (Show)

--
-- (using 'encode' from 10.hs)
encode :: (Eq a) => [a] -> [(Int, a)]
encode = foldr f []
  where
    f v [] = [(1, v)]
    f v (x:xs)
      | snd x == v = (fst x + 1, v) : xs
      | otherwise = (1, v) : x : xs

-- 'pattern matching' seems to be the only(?) way of accessing unnamed members of data struct...
encodeModified :: (Eq a) => [a] -> [Element a]
encodeModified xs = map f $ encode xs
  where
    f (1, v) = Single v
    f (n, v) = Multiple n v

--
-- (from the solution, use group)
encodeModified2 :: (Eq a) => [a] -> [Element a]
encodeModified2 = map f . group
  where
    f ys
      | length ys == 1 = Single (head ys)
      | otherwise = Multiple (length ys) (head ys)
