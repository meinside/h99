{--
https://wiki.haskell.org/99_questions/1_to_10

(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:

*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True

-- https://wiki.haskell.org/99_questions/Solutions/6
--}
--
-- (my solution)
isPalindrom :: Eq a => [a] -> Bool
isPalindrom xs
  | null xs = True
  | length xs == 1 = True
  | head xs == last xs = isPalindrom (init (tail xs))
  | otherwise = False
