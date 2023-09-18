import SetV1

import QueueV1

setPrueba :: Set Int
setPrueba = addS 3 emptyS

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _       = []
losQuePertenecen (x:xs) set = if belongs x set
                              then x : losQuePertenecen xs set
                              else losQuePertenecen xs set

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos lista = setToList (agregarListaAlSet lista)

agregarListaAlSet :: Eq a => [a] -> Set a
agregarListaAlSet []     = emptyS
agregarListaAlSet (x:xs) = addS x (agregarListaAlSet xs)

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

arbolEjemplo :: Tree (Set Int)
arbolEjemplo =
    NodeT (addS 1 (addS 2 emptyS))            -- Conjunto {1, 2}
        (NodeT (addS 3 emptyS) EmptyT EmptyT)  -- Conjunto {3}
        (NodeT emptyS EmptyT EmptyT)           -- Conjunto vacío


--data Tree Set a = EmptyT | NodeT Set a (Set a) (Set a)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos arbol = agregarListaAlSet (transformarArbolALista arbol)

transformarArbolALista :: Eq a => Tree (Set a) -> [a]
transformarArbolALista EmptyT = []
transformarArbolALista (NodeT set t1 t2) = setToList set ++ transformarArbolALista t1 ++ transformarArbolALista t2 

queuePrueba :: Queue Int
queuePrueba =  enqueue 3 emptyQ

lengthQ :: Queue a -> Int
--Cuenta la cantidad de elementos de la cola.
lengthQ q = if isEmptyQ q
            then 0
            else 1 + lengthQ (dequeue q)

