module BinomialHeap ( BinomialHeap ) where
  import Heap

  data Tree a = Node Int a [Tree a] deriving (Show)
  newtype BinomialHeap a = BH [Tree a]

  rank Node (r, _, _) = r

  root Node (_, x, _) = x

  link t1@Node(r, x1, c1) t2@(_, x2, c2) =
    | x1 < x2   = Node (r + 1, x1, t2 : c1)
    | otherwise = Node (r + 1, x2, t1 : c2)

  insTree (t, []) = ts1
  insTree (t, ts@t' : ts') =


