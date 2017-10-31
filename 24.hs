{--
https://wiki.haskell.org/99_questions/21_to_28

Lotto: Draw N different random numbers from the set 1..M.

Example in Haskell:

Prelude System.Random> diff_select 6 49
Prelude System.Random> [23,1,17,33,21,37]

-- https://wiki.haskell.org/99_questions/Solutions/24
--}
--
import System.Random

--
-- (my solution)
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  shuffled <- shuffle [1 .. m]
  return $ take n shuffled

--
-- (my shuffle function from 23.hs)
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  rndIdx <- randomRIO (1, length xs)
  let picked = xs !! (rndIdx - 1)
  rest <- shuffle [x | (x, i) <- zip xs [1 ..], i /= rndIdx]
  return (picked : rest)
