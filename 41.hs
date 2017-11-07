{--
https://wiki.haskell.org/99_questions/31_to_41

(**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

Example in Haskell:

*Exercises> goldbachList 9 20
[(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
*Exercises> goldbachList' 4 2000 50
[(73,919),(61,1321),(67,1789),(61,1867)]

-- https://wiki.haskell.org/99_questions/Solutions/41
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
-- (from 40.hs,)
goldbach :: Int -> (Int, Int)
goldbach n = head [(a, n - a) | a <- [2 .. n], isPrime a, isPrime (n - a)]

--
-- (my solution)
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList from to = [goldbach n | n <- [from .. to], even n]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' from to biggerThan =
  filter (\(a, _) -> a > biggerThan) $ goldbachList from to
