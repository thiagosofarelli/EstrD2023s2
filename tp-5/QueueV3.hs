module QueueV3 (
    Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue
) where

data Queue a = Queue [a] [a]
            -- Cola  Fs  Bs
    deriving Show
{--
Inv. Rep.: Si Fs es vacía, Bs es vacía.
--}

emptyQ :: Queue a
--Crea una cola vacía.
emptyQ = Queue [] []

isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
isEmptyQ (Queue fs _) = null fs

enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Queue fs bs) = (Queue  fs (x:bs))

firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola
--Es parcial, la cola debe tener al menos un elemento.
firstQ (Queue fs _) = head fs

dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.
dequeue (Queue fs bs) = if null fs 
                        then (dequeue (Queue (bs) []))
                            else if null (tail fs)
                            then Queue bs []
                            else (Queue (tail fs) bs)

queueEx = Queue [] [5,7,89]

reversa :: [a] -> [a]
reversa []      = []
reversa (x:xs)  = reversa xs ++ [x]



