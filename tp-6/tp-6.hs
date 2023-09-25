import PriorityQueueV1
import MapV1

--emptyM, assocM, lookupM, deleteM, keys
mapPrueba = assocM "Valija" "bolso con ropa" (assocM "Tesoro" "cofre de gran tamanio" emptyM)

valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = valores (keys m) m

valores :: Eq k => [k] -> Map k v -> [Maybe v]
valores [] _ = []
valores (k:ks) m = lookupM k m : valores ks m

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas ks m = pertenecen ks (keys m)

pertenecen :: Eq k => [k] -> [k] -> Bool
pertenecen [] _ = True
pertenecen (k:ks) xs = elem k xs && pertenecen ks xs

listToMap :: Eq k => [(k, v)] -> Map k v
--Propósito: convierte una lista de pares clave valor en un map.
listToMap [] = emptyM
listToMap ((k, v):kvs) = assocM k v (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k, v)]
--Propósito: convierte un map en una lista de pares clave valor.
mapToList m = asociarClaves (keys m) m

asociarClaves :: Eq k => [k] -> Map k v -> [(k,v)]
asociarClaves []     map = []
asociarClaves (k:ks) map = (k, fromJust(lookupM k map)) : asociarClaves ks map

fromJust :: Maybe v -> v
fromJust (Just v) = v

agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--la misma clave.
-- Costo cúbico?
agruparEq kvs  = asociarEqSubtarea (agruparEqSubTarea kvs)
   
asociarEqSubtarea :: Eq k => [(k, [v])] -> Map k [v]
-- Cuadrática
asociarEqSubtarea []        = emptyM
asociarEqSubtarea (kv:kvs)  = assocM (fst kv) (snd kv) (asociarEqSubtarea kvs)

agruparEqSubTarea :: Eq k => [(k, v)] -> [(k, [v])]
-- Cuadrática
agruparEqSubTarea []        = []
agruparEqSubTarea ((k, v):kvs)  = juntarValores (k, v) (agruparEqSubTarea kvs)
    
juntarValores :: Eq k => (k, v) -> [(k, [v])] -> [(k, [v])]
-- Lineal
juntarValores (k, v)  []      = [(k, [v])]
juntarValores (k, v) (kv:kvs) = if sonLaMismaClave k (fst kv)
                                then (fst kv, (v:(snd kv))) : kvs
                                else kv : juntarValores (k, v) kvs

sonLaMismaClave :: Eq k => k -> k -> Bool
--Constante
sonLaMismaClave k1 k2   = k1 == k2

--incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--cada número asociado con dichas claves.

--mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
