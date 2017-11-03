{--
https://wiki.haskell.org/99_questions/31_to_41

(**) Calculate Euler's totient function phi(m) (improved).

See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:

phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
         (p2 - 1) * p2 ** (m2 - 1) * 
         (p3 - 1) * p3 ** (m3 - 1) * ...
Note that a ** b stands for the b'th power of a.

-- https://wiki.haskell.org/99_questions/Solutions/37
--}
import Data.List

--
-- (from 36.hs)
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

--
-- (my solution)
phi :: Int -> Int
phi 1 = 1
phi n = foldl f 1 $ primeFactorsMult n
  where
    f acc factor = acc * (p - 1) * (p ^ (m - 1))
      where
        p = fst factor
        m = snd factor

--
-- (from the solution, using list comprehension!)
phi2 :: Int -> Int
phi2 m = product [(p - 1) * p ^ (c - 1) | (p, c) <- primeFactorsMult m]
