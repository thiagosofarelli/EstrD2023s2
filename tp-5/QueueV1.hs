module QueueV1 (
    Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue
) where

data Queue a = Queue [a]
    deriving Show

emptyQ :: Queue a
--Crea una cola vacía.
emptyQ = Queue []

isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
isEmptyQ (Queue []) = True
isEmptyQ _ = False


enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue e (Queue xs) = Queue (xs ++ [e]) 

firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola
firstQ (Queue [])   = error "La lista no puede ser vacía"
firstQ (Queue a)    = head a

dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.
dequeue (Queue [])  = error "La lista no puede ser vacía"
dequeue (Queue a)   = (Queue (tail a))



