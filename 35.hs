{--
https://wiki.haskell.org/99_questions/31_to_41

(**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

Example in Haskell:

> primeFactors 315
[3, 3, 5, 7]

-- https://wiki.haskell.org/99_questions/Solutions/35
--}
--
-- (my solution)
primeFactors :: Int -> [Int]
primeFactors number = reverse $ f number []
  where
    f 1 xs = xs
    f n xs = f (n `div` factor) (factor : xs)
      where
        factor = head [y | y <- [2 .. n], n `mod` y == 0]
