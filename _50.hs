{--
https://wiki.haskell.org/99_questions/46_to_50

(***) Huffman codes.

We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2 defined as follows:

% huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
Example in Haskell:

*Exercises> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
[('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

-- https://wiki.haskell.org/99_questions/Solutions/50
--}
--
-- (from the solution,)
-- XXX - not understood...
import qualified Data.List as L

huffman :: [(Char, Int)] -> [(Char, String)]
huffman x = reformat $ huffman_combine $ resort $ morph x
  where
    morph x = [([[]], [c], n) | (c, n) <- x]
    resort x = L.sortBy (\(_, _, a) (_, _, b) -> compare a b) x
    reformat (x, y, _) =
      L.sortBy (\(a, b) (x, y) -> compare (length b) (length y)) $ zip y x
    huffman_combine [x] = x
    huffman_combine (x:xs) =
      huffman_combine $ resort (combine_elements x (head xs) : tail xs)
      where
        combine_elements (a, b, c) (x, y, z) =
          (map ('0' :) a ++ map ('1' :) x, b ++ y, c + z)
