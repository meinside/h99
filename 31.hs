{--
https://wiki.haskell.org/99_questions/31_to_41

(**) Determine whether a given integer number is prime.

Example in Haskell:

P31> isPrime 7
True

-- https://wiki.haskell.org/99_questions/Solutions/31
--}
--
-- (my solution)
isPrime :: Int -> Bool
isPrime 1 = True
--isPrime n = foldl f True [2 .. (n - 1)]
isPrime n = foldl f True [2 .. floor $ sqrt $ fromIntegral (n - 1)]
  where
    f False _ = False
    f True v = n `mod` v /= 0
