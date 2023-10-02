{-- 
Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los
invariantes de BST y sin elementos repetidos (despreocuparse por el hecho de que el árbol puede
desbalancearse al insertar o borrar elementos). En todos los costos, N es la cantidad de elementos
del árbol. Justificar por qué la implementación satisface los costos dados.
--}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
{-- Inv. Rep.: BST
❏ todos los elementos de ti son menores que x
❏ todos los elementos de td son mayores que x
❏ ti y td también cumplen el invariante de BST
--}

arbol1 :: Tree Int
arbol1 = NodeT 15
            (NodeT 10
                (NodeT 5
                    (NodeT 2 EmptyT EmptyT)
                    (NodeT 8 EmptyT (NodeT 11 EmptyT EmptyT)))
                (NodeT 12 EmptyT EmptyT))
            (NodeT 25
                (NodeT 20
                    (NodeT 18 EmptyT EmptyT)
                    (NodeT 22 EmptyT EmptyT))
                (NodeT 30 EmptyT EmptyT))

arbol2 :: Tree Int
arbol2 =
  NodeT 15
    (NodeT 10
      (NodeT 5
        (NodeT 2 EmptyT EmptyT)
        (NodeT 8 EmptyT (NodeT 11 EmptyT EmptyT)))
      (NodeT 12 EmptyT EmptyT))
    (NodeT 25
      (NodeT 20
        (NodeT 18 EmptyT EmptyT)
        (NodeT 22 EmptyT EmptyT))
      (NodeT 30
        (NodeT 28
          (NodeT 26
            (NodeT 24
              (NodeT 23 EmptyT EmptyT)
              (NodeT 27 EmptyT EmptyT))
            EmptyT)
          EmptyT)
        EmptyT))

--1.
belongsBST :: Ord a => a -> Tree a -> Bool
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N) - Se recorre solo una rama de un árbol balanceado. (Inv. Rep.: El arbol se encuentra balanceado.)
belongsBST _ EmptyT = False
belongsBST x (NodeT e ti td) = 
    if x == e then True
    else if x < e 
        then belongsBST x ti
        else belongsBST x td 

--2.
insertBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST inserta un elemento en el árbol.
--Costo: O(log N) - Se recorre solo una rama de un árbol balanceado. (Inv. Rep.: El arbol se encuentra balanceado.)
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT e ti td) = 
    if x == e then NodeT x ti td
    else if x < e
        then NodeT e (insertBST x ti) td
        else NodeT e ti (insertBST x td)

--3. 
deleteBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST borra un elemento en el árbol.
--Costo: O(log N) - Se recorre solo una rama de un árbol balanceado. (Inv. Rep.: El arbol se encuentra balanceado.)
deleteBST _ EmptyT = EmptyT
deleteBST x (NodeT e ti td) = 
    if x == e then rearmarBST ti td
    else if x < e 
        then NodeT e (deleteBST x ti) td
        else NodeT e ti (deleteBST x td)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
 -- PRECOND: ambos árboles son BSTs
rearmarBST EmptyT td = td
rearmarBST ti td = NodeT (maxBST ti) (delMaxBST ti) td

maxBST :: Ord a => Tree a -> a
maxBST (NodeT x _ EmptyT) = x -- PRECOND: no es vacío
maxBST (NodeT _ _ td) = maxBST td

delMaxBST :: Ord a => Tree a -> Tree a
delMaxBST (NodeT _ ti EmptyT) = ti -- PRECOND: no es vacío
delMaxBST (NodeT x ti td) = NodeT x ti (delMaxBST td)

--4. 
splitMinBST :: Ord a => Tree a -> (a, Tree a)
--Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
--Costo: O(log N)
splitMinBST arbol = (minBST arbol, delMinBST arbol)

delMinBST :: Ord a => Tree a -> Tree a
delMinBST (NodeT x EmptyT td) = td
delMinBST (NodeT x ti td) = NodeT x (delMinBST ti) td 
  
minBST :: Ord a => Tree a -> a
minBST (NodeT x EmptyT _) = x -- PRECOND: no es vacío
minBST (NodeT x ti td) = minBST ti

--5. 
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
--Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
--Costo: O(log N)
splitMaxBST arbol = (maxBST arbol, delMaxBST arbol)

--6. 
esBST :: Ord a => Tree a -> Bool
--Propósito: indica si el árbol cumple con los invariantes de BST.
--Costo: O(N2)
esBST EmptyT                = True
esBST (NodeT x EmptyT EmptyT)= True
esBST (NodeT x EmptyT der)= x < maxBST der
esBST (NodeT x izq EmptyT)= x > maxBST izq
esBST (NodeT x izq der) = x > maxBST izq && x < minBST der && esBST der && esBST izq

--7. 
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un elemento y un BST, devuelve el máximo elemento que sea menor al
--elemento dado.
--Costo: O(log N)
elMaximoMenorA _ EmptyT = Nothing
elMaximoMenorA x (NodeT e ti td) = if x <= e && noEsEmptyT ti
                                    then elMaximoMenorA x ti
                                    else if x > e && noEsEmptyT td
                                        then elMaximoMenorA x td
                                        else if x > e
                                            then Just e
                                            else Nothing

noEsEmptyT :: Tree a -> Bool
noEsEmptyT EmptyT = False
noEsEmptyT _ = True

--8. 
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un elemento y un BST, devuelve el mínimo elemento que sea mayor al
--elemento dado.
--Costo: O(log N)
elMinimoMayorA _ EmptyT = Nothing
elMinimoMayorA x (NodeT e ti td) = undefined

--9.
balanceado :: Tree a -> Bool
--Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
--nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
--Costo: O(N2)
balanceado EmptyT            = True
balanceado (NodeT _ izq der) = abs ((altura izq) - (altura der)) <= 1 

altura :: Tree a -> Int
altura EmptyT = 0
altura (NodeT _ izq der) = 1 + max (altura izq) (altura der)
