{--
https://wiki.haskell.org/99_questions/31_to_41

(**) Goldbach's conjecture.

Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.

Example in Haskell:

*goldbach 28
(5, 23)

-- https://wiki.haskell.org/99_questions/Solutions/40
--}
--
-- (from 31.hs)
isPrime :: Int -> Bool
isPrime 1 = True
isPrime n = foldl f True [2 .. floor $ sqrt $ fromIntegral n]
  where
    f False _ = False
    f True v = n `mod` v /= 0

--
-- (my solution)
goldbach :: Int -> (Int, Int)
goldbach n = head [(a, n - a) | a <- [2 .. n], isPrime a, isPrime (n - a)]
