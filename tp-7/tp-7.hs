{-- 
Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los
invariantes de BST y sin elementos repetidos (despreocuparse por el hecho de que el árbol puede
desbalancearse al insertar o borrar elementos). En todos los costos, N es la cantidad de elementos
del árbol. Justificar por qué la implementación satisface los costos dados.
--}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
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

--1.
belongsBST :: Ord a => a -> Tree a -> Bool
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N)
belongsBST _ EmptyT = False
belongsBST x (NodeT e ti td) = 
    if x == e then True
    else if x < e 
        then belongsBST x ti
        else belongsBST x td 


--2.
--insertBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST inserta un elemento en el árbol.
--Costo: O(log N)

--3. 
--deleteBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST borra un elemento en el árbol.
--Costo: O(log N)

--4. 
--splitMinBST :: Ord a => Tree a -> (a, Tree a)
--Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
--Costo: O(log N)

--5. 
--splitMaxBST :: Ord a => Tree a -> (a, Tree a)
--Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
--Costo: O(log N)

--6. 
--esBST :: Tree a -> Bool
--Propósito: indica si el árbol cumple con los invariantes de BST.
--Costo: O(N2)

--7. 
--elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
--elemento dado.
--Costo: O(log N)

--8. 
--elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
--elemento dado.
--Costo: O(log N)

--9. 
--balanceado :: Tree a -> Bool
--Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
--nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
--Costo: O(N2)
