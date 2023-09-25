import MapV1

data MultiSet a = Ms (Map a Int)
    deriving Show

emptyMS :: MultiSet a
emptyMS = Ms emptyM

addMS :: Ord a => a -> MultiSet a -> MultiSet a
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
--multiconjunto.
addMS a (Ms map) = Ms (agregarAMS a map)

agregarAMS :: Ord a => a -> Map a Int -> Map a Int
agregarAMS elemento map = if (todasAsociadas [elemento] map)
                          then assocM elemento ((fromJust (lookupM elemento map)) + 1) map
                          else assocM elemento 1 map

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas ks m = pertenecen ks (keys m)

pertenecen :: Eq k => [k] -> [k] -> Bool
pertenecen [] _ = True
pertenecen (k:ks) xs = elem k xs && pertenecen ks xs

fromJust :: Maybe v -> v
fromJust (Just v) = v

ocurrencesMS :: Ord a => a -> MultiSet a -> Int
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
--elemento en el multiconjunto.
--Precondición: El elemento dado está en el multiconjunto.
ocurrencesMS elemento (Ms map) = fromJust (lookupM elemento map)

multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
--su cantidad de ocurrencias.
multiSetToList (Ms map) = mapToList map

mapToList :: Eq k => Map k v -> [(k, v)]
--Propósito: convierte un map en una lista de pares clave valor.
mapToList m = asociarClaves (keys m) m

asociarClaves :: Eq k => [k] -> Map k v -> [(k,v)]
asociarClaves []     map = []
asociarClaves (k:ks) map = (k, fromJust(lookupM k map)) : asociarClaves ks map

msPrueba = Ms (assocM "Valija" 3 (assocM "Tesoro" 8 emptyM))