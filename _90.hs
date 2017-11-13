{--
https://wiki.haskell.org/99_questions/90_to_94

(**) Eight queens problem

This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.

Hint: Represent the positions of the queens as a list of numbers 1..N. Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.

Example in Haskell:

> length (queens 8)
92
> head (queens 8)
[1,5,8,6,3,7,2,4]

-- https://wiki.haskell.org/99_questions/Solutions/90
--}
--
-- (from the solution, not efficient)
queens :: Int -> [[Int]]
queens n = filter check (allPermu n)
  where
    allPermu 0 = [[]]
    allPermu m = [q : qs | q <- [1 .. n], qs <- allPermu (m - 1)]
    check [] = True
    check (q:qs) = cantAttack q qs && check qs
    cantAttack q qs = not (q `elem` qs || sameDiag q qs)
    sameDiag q qs =
      any (\(column, queen) -> abs (q - queen) == column) $ zip [1 ..] qs

-- (from the solution,)
queens2 :: Int -> [[Int]]
queens2 n = map reverse $ queens' n
  where
    queens' 0 = [[]]
    queens' k = [q : qs | qs <- queens' (k - 1), q <- [1 .. n], isSafe q qs]
    isSafe try qs = not (try `elem` qs || sameDiag try qs)
    sameDiag try qs =
      any (\(colDist, q) -> abs (try - q) == colDist) $ zip [1 ..] qs
