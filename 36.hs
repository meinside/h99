{--
https://wiki.haskell.org/99_questions/31_to_41

(**) Determine the prime factors of a given positive integer.

Construct a list containing the prime factors and their multiplicity.

Example in Haskell:

*Main> prime_factors_mult 315
[(3,2),(5,1),(7,1)]

-- https://wiki.haskell.org/99_questions/Solutions/36
--}
--
import Data.List

--
-- (my solution, using 'group')
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult m = map (\x -> (head x, length x)) $ group $ primeFactors m
  where
    primeFactors :: Int -> [Int]
    primeFactors number = reverse $ f number []
      where
        f 1 xs = xs
        f n xs = f (n `div` factor) (factor : xs)
          where
            factor = head [y | y <- [2 .. n], n `mod` y == 0]
