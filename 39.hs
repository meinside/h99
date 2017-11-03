{--
https://wiki.haskell.org/99_questions/31_to_41

(*) A list of prime numbers.

Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

Example in Haskell:

P39> primesR 10 20
[11,13,17,19]

-- https://wiki.haskell.org/99_questions/Solutions/39
--}
--
-- (from 31.hs)
isPrime :: Int -> Bool
isPrime 1 = True
--isPrime n = foldl f True [2 .. (n - 1)]
isPrime n = foldl f True [2 .. floor $ sqrt $ fromIntegral (n - 1)]
  where
    f False _ = False
    f True v = n `mod` v /= 0

--
-- (my solution)
primesR :: Int -> Int -> [Int]
primesR a b = [x | x <- [a .. b], isPrime x]
