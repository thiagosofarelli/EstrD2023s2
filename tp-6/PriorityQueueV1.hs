module PriorityQueueV1 (
    PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ
) where

data PriorityQueue a = PriorityQueue [a]
    deriving Show

emptyPQ :: PriorityQueue a
--Propósito: devuelve una priority queue vacía.
emptyPQ = PriorityQueue [] -- Costo o(1) - Constante

isEmptyPQ :: PriorityQueue a -> Bool
--Propósito: indica si la priority queue está vacía.
isEmptyPQ (PriorityQueue xs) = null xs -- Costo o(1) - Constante

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
--Propósito: inserta un elemento en la priority queue.
insertPQ e (PriorityQueue xs) = PriorityQueue (e:xs) -- Costo o(1) - Constante

findMinPQ :: Ord a => PriorityQueue a -> a
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMinPQ (PriorityQueue xs) = minimum xs -- Costo o(n) - Lineal

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ (PriorityQueue xs) = PriorityQueue (borrarMin xs) -- Costo o(n) - Lineal

borrarMin :: Ord a => [a] -> [a]
--Precondición: la lista no es vacía.
borrarMin xs = borrar (minimum xs) xs -- Costo o(n) - Lineal

borrar :: Eq a => a -> [a] -> [a]
borrar a []     = []
borrar a (x:xs) = if a == x
                  then xs
                  else x : borrar a xs -- Costo o(n) - Lineal
                  