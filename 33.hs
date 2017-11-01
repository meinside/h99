{--
https://wiki.haskell.org/99_questions/31_to_41

(*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

Example in Haskell:

* coprime 35 64
True

-- https://wiki.haskell.org/99_questions/Solutions/33
--}
--
-- (my solution, using 'gcd')
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1
