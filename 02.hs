{--
https://wiki.haskell.org/99_questions/1_to_10

(*) Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'

-- https://wiki.haskell.org/99_questions/Solutions/2
--}
--
-- (my solution)
myButLast :: [a] -> a
myButLast xs = last $ init xs
--myButLast = last . init
