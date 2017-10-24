{--
https://wiki.haskell.org/99_questions/1_to_10

(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

Example in Haskell:

*Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]

-- https://wiki.haskell.org/99_questions/Solutions/9
--}
--
--(my solution)
pack :: (Eq a) => [a] -> [[a]]
--pack xs =
--  foldr
--    (\e acc ->
--       if null (head acc)
--         then [e] : tail acc
--         else (if head (head acc) == e
--                 then (e : head acc) : tail acc
--                 else [e] : acc))
--    [[]]
--    xs
pack xs = foldr func [] xs
  where
    func x [] = [[x]]
    func x (y:xs) =
      if x == head y
        then (x : y) : xs
        else [x] : y : xs
