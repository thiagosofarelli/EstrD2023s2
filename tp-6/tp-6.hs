import PriorityQueueV1
import MapV1
--import MapV2 -- Con dos listas

--emptyM, assocM, lookupM, deleteM, keys

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
agruparEq []       = emptyM
agruparEq (kv:kvs) = asociarConLista kv (agruparEq kvs)

asociarConLista :: Eq k => (k, v) -> Map k [v] -> Map k [v]
asociarConLista (k, v) map = if noEsNothing(lookupM k map)
                             then assocM k (v:(fromJust (lookupM k map) )) map
                             else assocM k [v] map
    
noEsNothing :: Maybe v -> Bool
noEsNothing Nothing = False
noEsNothing _       = True

sonLaMismaClave :: Eq k => k -> k -> Bool
--Constante
sonLaMismaClave k1 k2   = k1 == k2

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--cada número asociado con dichas claves.
incrementar [] m     = m
incrementar (k:ks) m = if elem k (keys m)
                       then assocM k (fromJust (lookupM k m)+1) (incrementar ks m)
                       else incrementar ks m

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps m1 m2 = agregarClavesYValores (mapToList m1) m2

agregarClavesYValores :: Eq k => [(k, v)] -> Map k v -> Map k v
agregarClavesYValores [] m = m
agregarClavesYValores ((k, y):kvs) m = assocM k y (agregarClavesYValores kvs m)
{--
mapPrueba = assocM "Valija" "bolso con ropa" (assocM "Tesoro" "cofre de gran tamanio" emptyM)
mapPrueba2 = assocM "Valija" "ropita" (assocM "Auto" "Lamborghini" emptyM)

indexar :: [a] -> Map Int a
--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
--su posición en la lista.
indexar [] = emptyM
indexar xs = indexarDesde 1 xs  

indexarDesde :: Int -> [a] -> Map Int a
indexarDesde _ [] = emptyM
indexarDesde n (x:xs) = assocM n x (indexarDesde (n+1) xs)


-- Falta modificar el siguiente error:
{-- Si llamo a OCURRENCIAS HOOLA, me va a devolver
un Map en el que una letra "O" va a tener el número 2,
y otra letra "O" va a tener el número 1.--}

ocurrencias :: String -> Map Char Int
ocurrencias []           = emptyM
ocurrencias (char:chars) = assocM char ((apariciones char chars) + 1) (ocurrencias chars)

apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0
apariciones e (x:xs) = if e == x
                         then 1 + apariciones e xs
                         else apariciones e xs
                         --}
