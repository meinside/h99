{--
https://wiki.haskell.org/99_questions/11_to_20

(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example in Haskell:

P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']

-- https://wiki.haskell.org/99_questions/Solutions/13
--}
--
data Element a
  = Multiple Int
             a
  | Single a
  deriving (Show)

--
-- (my solution)
encodeDirect :: (Eq a) => [a] -> [Element a]
--encodeDirect = foldr f []
--  where
--    f v [] = [Single v]
--    f v (Single x:xs)
--      | v == x = Multiple 2 x : xs
--      | otherwise = Single v : Single x : xs
--    f v (Multiple n x:xs)
--      | v == x = Multiple (n + 1) x : xs
--      | otherwise = Single v : Multiple n x : xs
--
--
-- (my solution - shortened)
encodeDirect = foldr f []
  where
    f v [] = [Single v]
    f v (s@(Single x):xs)
      | v == x = Multiple 2 x : xs
      | otherwise = Single v : s : xs
    f v (m@(Multiple n x):xs)
      | v == x = Multiple (n + 1) x : xs
      | otherwise = Single v : m : xs
