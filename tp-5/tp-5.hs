import SetV1

import QueueV2

import StackV1

head' :: [a] -> a
head' (x:xs) = x

{- La función tiene un costo constante / O(1) -}

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

{- La función tiene un costo constante / O(1) -}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

{- La función tiene un costo lineal  / O(n) -}

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

{- La función tiene un costo lineal  / O(n) -}

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

{- La función tiene un costo cuadrático / O(n^2) -}

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

{- La función tiene un costo lineal  / O(n) -}
{-
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs
-}

{- La función tiene un costo cuadrático / O(n^2) -}

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

{- La función tiene un costo lineal  / O(n) -}

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

{- La función tiene un costo cuadrático  / O(n^2) -}

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

{- La función tiene un costo lineal  / O(n) -}

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

{- La función tiene un costo lineal  / O(n) -}

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

{- La función tiene un costo cuadrático / O(n^2) -}

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

{- La función tiene un costo lineal  / O(n) -}

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                 then xs
                 else x : sacar n xs

{- La función tiene un costo lineal  / O(n) -}

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
    let m = minimo xs
        in m : ordenar (sacar m xs)

{- La función tiene un costo cuadrático / O(n^2) -}

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
queuePrueba =  enqueue 9 (enqueue 3 emptyQ)

queuePrueba2 :: Queue Int
queuePrueba2 =  enqueue 218 (enqueue 12 (enqueue 9 emptyQ))

lengthQ :: Queue a -> Int
--Cuenta la cantidad de elementos de la cola.
lengthQ q = if isEmptyQ q
            then 0
            else 1 + lengthQ (dequeue q)

queueToList :: Queue a -> [a]
--Dada una cola devuelve la lista con los mismos elementos,
--donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
queueToList queue = if isEmptyQ queue
                    then []
                    else firstQ queue : queueToList (dequeue queue)

unionQ :: Queue a -> Queue a -> Queue a
--Inserta todos los elementos de la segunda cola en la primera.
unionQ q1 q2 = enqueueAllOf_To_ (queueToList q2) q1

enqueueAllOf_To_ :: [a] -> Queue a -> Queue a
enqueueAllOf_To_ [] q1     = q1
enqueueAllOf_To_ (x:xs) q1 = enqueue x (enqueueAllOf_To_ xs q1)

stackPrueba :: Stack Int
stackPrueba =  push 9 (push 3 emptyStack)

stackPrueba2 :: Stack Int
stackPrueba2 =  push 218 (push 12 (push 9 emptyStack))

apilar :: [a] -> Stack [a]
apilar [] = emptyStack
apilar (x:xs) = push x (apilar xs)
