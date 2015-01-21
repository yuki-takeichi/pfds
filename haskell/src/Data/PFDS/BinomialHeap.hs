module Data.PFDS.BinomialHeap (
  BinomialHeap(..),
  rank,
  root,
  link,
  insTree,
  insert,
  removeMinTree,
  findMin,
  deleteMin
) where

  import Data.PFDS.Heap
  import Control.Monad.Error

  data Tree a = Node Int a [Tree a] deriving (Show)
  newtype BinomialHeap a = BH [Tree a]

  rank Node (r, _, _) = r

  root Node (_, x, _) = x

  link t1@Node(r, x1, c1) t2@(_, x2, c2)
    | x1 < x2   = Node (r + 1, x1, t2:c1)
    | otherwise = Node (r + 1, x2, t1:c2)

  insTree (t, []) = ts1
  insTree (t, ts@t' : ts')
    | rank t < rank t' = t:ts
    | otherwise = insTree (link (t, t'), ts')

  insert (x, ts) = insTree (Node (0, x, []), ts)

  merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
  merge (ts1, []) = ts1
  merge ([], ts2) = ts2
  merge (ts1@t1:ts1', ts2@t2:ts2')
    | rank t1 < rank t2 = merge (ts1', ts2)
    | rank t2 < rank t1 = merge (ts1, ts2')
    | othrewise = insTree (link (t1, t2), merge (ts1', ts2'))

  removeMinTree [] = Error "empty"
  removeMinTree [t] = (t, [])
  removeMinTree (t:s)
   | root t < root t' = (t, ts)
   | otherwise = (t', t:ts')
  
  findMin :: Ord a => BinomialHeap a -> a
  findMin ts = let (t, _) = removeMinTree ts in root t

  deleteMin :: Ord a => BinomialHeap a -> BinomialHeap a
  deleteMin ts = let (Node (_, x, ts1), ts2) = removeMinTree ts
                 in merge (rev ts1, ts2)

