{--
https://wiki.haskell.org/99_questions/31_to_41

(**) Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

Example in Haskell:

* totient 10
4

-- https://wiki.haskell.org/99_questions/Solutions/34
--}
--
-- (my solution, using 'gcd')
totient :: Int -> Int
totient 1 = 1
totient m = length $ filter f [1 .. m]
  where
    f x = gcd x m == 1
