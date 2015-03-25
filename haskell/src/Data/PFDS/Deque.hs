module Data.PFDS.Deque (Deque(..)) where

import Prelude hiding (head, tail, last, init)

class Queue q => Deque where
  cons :: a -> q a -> q a
  last :: q a -> a
  init :: q a -> q a
