{--
https://wiki.haskell.org/99_questions/11_to_20

(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

Example in Haskell:

P12> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"

-- https://wiki.haskell.org/99_questions/Solutions/12
--}
--
data Element a
  = Multiple Int
             a
  | Single a
  deriving (Show)

-- (my solution)
decodeModified :: (Eq a) => [Element a] -> [a]
decodeModified = foldr f []
  where
    f (Multiple n v) acc = replicate n v ++ acc
    f (Single v) acc = v : acc
