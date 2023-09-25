module MapV1 (
    Map, emptyM, assocM, lookupM, deleteM, keys
) where

data Map k v = M [(k, v)]
    deriving Show

emptyM :: Map k v
--Propósito: devuelve un map vacío.
emptyM = M [] -- O(1)

-- De esta manera, solo hay UNA CLAVE junto a SU ASOCIACIÓN. Es decir, no hay repetidas.
assocM :: Eq k => k -> v -> Map k v -> Map k v
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M kvs) = M (asociar k v kvs)

asociar :: Eq k => k -> v -> [(k, v)] -> [(k, v)] -- O(n)
asociar k v [] = [(k, v)]
asociar k v ((k', v'): kvs) = if k == k'
                              then (k', v):kvs
                              else (k', v') : asociar k v kvs
 

-- De esta manera, puede haber claves repetidas, con distinta o misma asociación cada una.
{--assocM :: Eq k => k -> v -> Map k v -> Map k v -- O(1)
--Propósito: agrega una asociación clave-valor al map.
-- Costo O(1)
assocM k v (M kvs) = M ((k, v):kvs)--}

lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.
lookupM key (M kvs) = buscar key kvs

buscar:: Eq k => k -> [(k,v)] -> Maybe v -- O(n)
buscar k       []         = Nothing
buscar k ((k', v'):kvs)   = if k == k'
                            then (Just v')
                            else buscar k kvs

deleteM :: Eq k => k -> Map k v -> Map k v
--Propósito: borra una asociación dada una clave.
deleteM k (M kvs) = M (delete k kvs)

delete :: Eq k => k -> [(k, v)] -> [(k, v)] -- O(n)
delete _ []      = []
delete k ((k', v'):kvs)= if k == k'
                             then delete k kvs
                             else (k', v') : delete k kvs

keys :: Map k v -> [k]
--Propósito: devuelve las claves del map.
keys (M kvs) = keysL kvs
keysL :: [(k, v)] -> [k] -- O(n)
keysL       []     = []
keysL ((k, v):kvs) = k : keysL kvs

mapPrueba = assocM "Valija" "bolso con ropa" (assocM "Tesoro" "cofre de gran tamanio" emptyM)