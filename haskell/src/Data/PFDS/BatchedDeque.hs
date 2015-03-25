module BatchedDeque (BatchedDeque)

import Prelude hiding (head, tail, last, init)
import Data.PFDS.Deque

data = BatchedDeque a = BD [a] [a]

check [] r  = BD (reverse r) []
check f  [] = BD [] (reverse f)
check f  r  = BD f r

instance Deque BatchedDeque where
  empty = BD [] []
  isEmpty (BQ f r) = null f && null r
