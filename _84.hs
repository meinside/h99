{--
https://wiki.haskell.org/99_questions/80_to_89

(**) Construct the minimal spanning tree

Write a predicate ms_tree(Graph,Tree,Sum) to construct the minimal spanning tree of a given labelled graph. Hint: Use the algorithm of Prim. A small modification of the solution of P83 does the trick. The data of the example graph to the right can be found in the file p84.dat.

Example in Haskell:

prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
[(1,2,12),(1,3,34),(2,4,55),(2,5,32)]

-- https://wiki.haskell.org/99_questions/Solutions/84
--}
--
import Data.Array
import Data.List

--
--
type Graph n w = Array n [(n, w)]

--
-- (from the solution,)
-- XXX - ??? 
mkGraph dir bnds es =
  accumArray
    (\xs x -> x : xs)
    []
    bnds
    ([(x1, (x2, w)) | (x1, x2, w) <- es] ++
     if dir
       then []
       else [(x2, (x1, w)) | (x1, x2, w) <- es, x1 /= x2])

adjacent g v = map fst (g ! v)

nodes g = indices g

edgeIn g (x, y) = elem y (adjacent g x)

weight x y g = head [c | (a, c) <- g ! x, a == y]

edgesD g = [(v1, v2, w) | v1 <- nodes g, (v2, w) <- g ! v1]

edgesU g = [(v1, v2, w) | v1 <- nodes g, (v2, w) <- g ! v1, v1 < v2]

prim g = prim' [n] ns []
  where
    (n:ns) = nodes g
    es = edgesU g
    prim' t [] mst = mst
    prim' t r mst =
      let e@(c, u', v') =
            minimum [(c, u, v) | (u, v, c) <- es, elem u t, elem v r]
      in prim' (v' : t) (delete v' r) (e : mst)
