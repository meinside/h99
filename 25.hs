{--
https://wiki.haskell.org/99_questions/21_to_28

Generate a random permutation of the elements of a list.

Example in Haskell:

Prelude System.Random> rnd_permu "abcdef"
Prelude System.Random> "badcef"

-- https://wiki.haskell.org/99_questions/Solutions/25
--}
--
import System.Random

--
-- (my solution, from 23.hs)
rndPermu :: [a] -> IO [a]
rndPermu [] = return []
rndPermu xs = do
  rndIdx <- randomRIO (1, length xs)
  let picked = xs !! (rndIdx - 1)
  rest <- rndPermu [x | (x, i) <- zip xs [1 ..], i /= rndIdx]
  return (picked : rest)

--
-- (my solution, a little faster)
rndPermu2 :: [a] -> IO [a]
rndPermu2 [] = return []
rndPermu2 xs = do
  rndIdx <- randomRIO (0, length xs - 1)
  let picked = xs !! rndIdx
  rest <- rndPermu2 (take rndIdx xs ++ drop (rndIdx + 1) xs)
  return (picked : rest)
