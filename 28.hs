{--
https://wiki.haskell.org/99_questions/21_to_28

Sorting a list of lists according to length of sublists

a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

Example in Haskell:

Prelude> lsort ["abc","de","fgh","de","ijkl","mn","o"]
Prelude> ["o","de","de","mn","abc","fgh","ijkl"]

b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

Example in Haskell:

Prelude> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
Prelude> ["ijkl","o","abc","fgh","de","de","mn"]

-- https://wiki.haskell.org/99_questions/Solutions/28
--}
import Data.List

--
-- (my solution, using quick sort)
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) =
  lsort [y | y <- xs, length y < length x] ++
  [x] ++ lsort [z | z <- xs, length z >= length x]

-- (my solution, not efficient)
lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort w@(x:xs) =
  lfsort [y | y <- xs, freq y < freq x] ++
  [x] ++ lfsort [z | z <- xs, freq z >= freq x]
  where
    freq :: [a] -> Int
    freq as = length [b | b <- w, length b == length as]

-- (from the solution, using groupBy)
lfsort2 :: [[a]] -> [[a]]
lfsort2 [] = []
lfsort2 xs = concat $ lsort $ groupBy (\a b -> length a == length b) $ lsort xs
