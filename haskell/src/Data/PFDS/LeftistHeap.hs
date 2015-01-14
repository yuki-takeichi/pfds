module LeftistHeap ( LeftistHeap ) where
  import Heap

  data LeftistHeap a = E | T Int a ( LeftistHeap a ) ( LeftistHeap a )
                       deriving (Show)
  rank E = 0
  rank ( T r _ _ _ ) = r

  makeT x a b
    | rank a >= rank b = T ( rank b + 1 ) x a b
    | otherwise        = T ( rank a + 1 ) x b a

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
