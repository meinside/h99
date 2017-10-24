{--
https://wiki.haskell.org/99_questions/1_to_10

(*) Find the last element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'

-- https://wiki.haskell.org/99_questions/Solutions/1
--}
--
-- (my solution)
myLast :: [a] -> a
myLast [] = error "Empty list given"
myLast [a] = a
myLast (_:xs) = myLast xs
