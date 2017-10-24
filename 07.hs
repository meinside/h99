{--
https://wiki.haskell.org/99_questions/1_to_10

(**) Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example in Haskell:

We have to define a new data type, because lists in Haskell are homogeneous.

data NestedList a = Elem a | List [NestedList a]

*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]

-- https://wiki.haskell.org/99_questions/Solutions/7
--}
data NestedList a
  = Elem a
  | List [NestedList a]

-- (my solution, not efficient)
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
