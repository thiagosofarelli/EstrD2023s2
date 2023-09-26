module QueueV2 (
    Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue
) where

data Queue a = Queue [a]
    deriving Show

emptyQ :: Queue a
--Crea una cola vacía.
emptyQ = Queue []

isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
isEmptyQ (Queue xs) = null xs


enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue e (Queue xs) = Queue (e:xs) 

firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola
--Es parcial, la cola debe tener al menos un elemento.
firstQ (Queue a) = elUltimo a

elUltimo :: [a] -> a
--Es parcial, debe haber al menos un elemento en la lista
elUltimo    []  = error "La cola no tiene al menos un elemento"
elUltimo (x:[]) = x
elUltimo (x:xs) = elUltimo xs

dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.
dequeue (Queue a) = Queue (sinElUltimo a)

sinElUltimo :: [a] -> [a]
-- Es parcial, debe haber al menos un elemento en la lista
sinElUltimo     [] = error "La cola no tiene al menos un elemento"
sinElUltimo (x:[]) = []
sinElUltimo (x:xs) = x : sinElUltimo xs
