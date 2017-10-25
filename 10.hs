{--
https://wiki.haskell.org/99_questions/1_to_10

(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example in Haskell:

encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
--}
--
encode :: (Eq a) => [a] -> [(Int, a)]
-- (my first solution)
--encode xs = foldl f [] xs
--  where
--    f acc x
--      | null acc = [(1, x)]
--      | snd (last acc) == x = init acc ++ [(fst (last acc) + 1, x)]
--      | otherwise = acc ++ [(1, x)]
--
-- (my second solution, using foldr)
encode = foldr f []
  where
    f v [] = [(1, v)]
    f v (x:xs)
      | snd x == v = (fst x + 1, v) : xs
      | otherwise = (1, v) : x : xs

--
-- (using 'pack' from 09.hs)
pack :: (Eq a) => [a] -> [[a]]
pack = foldr func []
  where
    func x [] = [[x]]
    func x (y:xs) =
      if x == head y
        then (x : y) : xs
        else [x] : y : xs

encode2 xs = map (\s -> (length s, head s)) $ pack xs
