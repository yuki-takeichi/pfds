use "bst.sml";

signature HEAP =
sig
  structure Elem : ORDERED
  type Heap
  val empty : Heap
  val isEmpty : Heap -> bool

  val insert : Elem.T * Heap -> Heap
  val merge : Heap * Heap -> Heap

  val findMin : Heap -> Elem.T
  val deleteMin : Heap -> Heap
end

functor LeftistHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element
  datatype Heap = E | T of int * Elem.T * Heap * Heap

  fun rank E = 0
    | rank (T (r, _, _, _)) = r
  fun makeT (x, a, b) = if rank a >= rank b then T (rank b + 1, x, a, b)
                        else T (rank a + 1, x, b, a)

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (h, E) = h
    | merge (E, h) = h
    | merge (h1 as T (_, x, a1, b1), h2 as T (_, y, a2, b2)) =
        if Elem.leq (x, y) then makeT (x, a1, merge (b1, h2))
        else makeT (y, a2, merge (h1, b2))

  (* fun insert (x, h) = merge (T (1, x, E, E), h) *)
  fun insert (x, E) = T (1, x, E, E)
  fun insert (x, h as T (_, y, a, b)) = 
        if Elem.leq (x, y) then makeT (x, E, merge (E, h))
        else makeT (y, a, insert (x, b))

  exception EMPTY
  fun findMin E = raise EMPTY
    | findMin (T (_, x, a, b)) = x
  fun deleteMin E = raise EMPTY
    | deleteMin (T (_, x, a, b)) = merge (a, b)

  (* exercise 3.3 *)
  fun fromList [] = E
end

fun pop x:xs = (x, xs)
fun eat 0 xs = (E, xs)
    eat 1 xs = let (x1, xs1) = pop xs,
                   (x2, xs2) = pop xs1
               in (merge(x1, x2), xs2)
fun fromList' xs 0 = eat (0, xs)


structure IntLeftistHeap = LeftistHeap (IntElem)

