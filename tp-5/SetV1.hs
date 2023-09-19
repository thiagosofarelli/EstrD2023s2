module SetV1 (
    Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList
) where

data Set a = Set [a]
    deriving Show

{- INV. REP.: En Set ls se cumple que
    * ls no posee elementos repetidos.
-}

emptyS :: Set a
emptyS = Set []

addS :: Eq a => a -> Set a -> Set a
addS e (Set a) = if belongs e (Set a)
                 then Set a
                 else Set (e:a) 

belongs :: Eq a => a -> Set a -> Bool
belongs e (Set a) = elem e a

sizeS :: Eq a => Set a -> Int
sizeS (Set a) = lengthSinRepetidos a

removeS :: Eq a => a -> Set a -> Set a
removeS e (Set a) = Set (removeE e a)

unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS (Set xs) (Set ys) = Set (sinRepetidos (xs ++ ys))

setToList :: Eq a => Set a -> [a]
setToList (Set a) = sinRepetidos a

-- Funciones que el usuario NO puede usar
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False
pertenece e (x:xs) = e == x || pertenece e xs


lengthSinRepetidos :: Eq a => [a] -> Int
lengthSinRepetidos (x:xs) = if pertenece x xs
                            then lengthSinRepetidos xs
                            else 1 + lengthSinRepetidos xs

removeE :: Eq a => a -> [a] -> [a]
removeE _ []     = []
removeE e (x:xs) = if e == x 
                   then xs
                   else x : removeE e xs

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if pertenece x xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs