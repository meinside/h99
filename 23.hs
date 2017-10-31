{--
https://wiki.haskell.org/99_questions/21_to_28

Extract a given number of randomly selected elements from a list.

Example in Haskell:

Prelude System.Random > rnd_select "abcdefgh" 3 >>= putStrLn
eda

-- https://wiki.haskell.org/99_questions/Solutions/23
--}
--
import System.Random

--
-- (my solution)
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
  shuffled <- shuffle xs
  return $ take n shuffled

--
-- (my shuffle function, monad is too hard...)
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  rndIdx <- randomRIO (1, length xs)
  let picked = xs !! (rndIdx - 1)
  rest <- shuffle [x | (x, i) <- zip xs [1 ..], i /= rndIdx]
  return (picked : rest)
