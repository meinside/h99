{--
https://wiki.haskell.org/99_questions/31_to_41

(**) Determine the greatest common divisor of two positive integer numbers. Use [Euclid's algorithm](http://en.wikipedia.org/wiki/Euclidean_algorithm).

Example in Haskell:

[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
[9,3,3]

-- https://wiki.haskell.org/99_questions/Solutions/32
--}
--
-- (my solution)
myGcd :: Int -> Int -> Int
myGcd a b
  | a < 0 || b < 0 = myGcd (abs a) (abs b)
  | a < b = myGcd b a
  | a `mod` b == 0 = b
  | otherwise = myGcd b (a `mod` b)
