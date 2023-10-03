{--
Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está
asociada al valor en la misma posición, pero de la otra lista.
--}

module MapV2 (
    Map, emptyM, assocM, lookupM, deleteM, keys
) where

data Map k v = M [k] [v]
-- Inv. Rep: La clave ubicada en i de K, está asociada con el valor i de V.
    deriving Show

mapPrueba = M [1,2,3,4,5,6,7] ["Auto","Puerta","Ventana","Calor","Invierno","PC","Mouse"] 

emptyM :: Map k v
--Propósito: devuelve un map vacío.
emptyM = M [] [] -- O(1)

assocM :: Eq k => k -> v -> Map k v -> Map k v
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M ks vs) = M (k:ks) (v:vs)

lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.
lookupM key (M ks vs) = buscar key ks vs

buscar:: Eq k => k -> [k] -> [v] -> Maybe v -- O(n)
buscar key [] _ = Nothing
buscar key (k:ks) (vs) = if key == k
                           then Just (head vs)
                           else buscar key ks (tail vs)

deleteM :: Eq k => k -> Map k v -> Map k v
--Propósito: borra una asociación dada una clave.
deleteM key (M ks vs) = M (deleteKey key ks) (deleteAssoc (pasosHasta key ks) vs)

deleteKey :: Eq k => k -> [k] -> [k]
deleteKey key []     = []
deleteKey key (k:ks) = if key == k
                       then ks
                       else k : deleteKey key ks

deleteAssoc :: Int -> [a] -> [a]
deleteAssoc 0 (x:xs) = xs
deleteAssoc _ [] = []
deleteAssoc n (x:xs) = x : deleteAssoc (n-1) xs 

pasosHasta :: Eq a => a -> [a] -> Int
-- Precondición: a existe en [a]
pasosHasta a (x:xs) = if a == x
                      then 0
                      else 1 + pasosHasta a xs
                            
keys :: Map k v -> [k]
--Propósito: devuelve las claves del map.
keys (M ks vs) = ks


