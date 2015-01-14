module WeightBiasedLeftistHeap ( WeightBiasedLeftistHeap ) where
  import Heap

  data WeightBiasedLeftistHeap a = E | T Int a ( WeightBiasedLeftistHeap a ) ( WeightBiasedLeftistHeap ) deriving (Show)

  size E = 0
  size ( T s _ _ _ ) = s

  makeT x a b
    | size a >= size b = T ( size a + size b + 1 ) x a b
    | otherwise        = T ( size a + size b + 1 ) x b a

  instance Heap LeftistHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    insert x h = merge ( T 1 x E E ) h

    merge h E = h
    merge E h = h
    merge h0@( T _ x a0 b0 ) h1@( T _ x a1 b1 )
      | x <= y    = makeT x a0 ( merge b0 h1 )
      | otherwise = makeT y a1 ( merge h0 b1 )

    findMin E = error "empty heap"
    findMin ( T _ x a b ) = x

    deleteMin E = error "empty heap"
    deleteMin ( T _ x a b ) = merge a b
