{--
CONSTANTE       - Siempre el mismo costo                            O(1)
LOGARÍTMICO     - Búsqueda en un árbol                              O(log n)
LINEAL          - Solo operaciones constantes por elemento          O(n)
ENELOGUENE      - Hasta operaciones logarítmicas por elemento       O(n log n)
CUADRÁTICA      - Hasta operaciones lineales por elemento           O(n^2)

TADs a conocer: Stacks, Queues, Priority Queues, Maps, Multisets.
Implementaciones: BSTs, AVLs, Heaps.
--}

-- MAGO -- 

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)

{-- Inv. Rep.:
- Sea EDM sh mnm pqm
- El mago 'x' en mnm, tiene como nombre 'x'.
- Todos los magos de 'mnm' se encuentran en 'pqm' y viceversa.
- Todos los magos de 'mnm' pueden solo poseer hechizos que en encuentran en 'sh'.
- Todos los hechizos que sabe el mago con nombre 'x' de 'mnm', son los mismos 
  hechizos que sabe el mago con nombre 'x' de 'pqm'.
- En 'pqm' no hay magos con el mismo nombre.

--}

{--
Mago, siendo H la cantidad de hechizos que sabe:
crearM :: Nombre -> Mago O(1)
nombre :: Mago -> Nombre O(1)
aprender :: Hechizo -> Mago -> Mago O(log H)
hechizos :: Mago -> Set Hechizo O(1)

Set, siendo N la cantidad de elementos del conjunto:
emptyS :: Set a O(1)
addS :: Ord a => a -> Set a -> Set a O(log N)
belongsS :: Ord a => a -> Set a -> Bool O(log N)
unionS :: Ord a => Set a -> Set a -> Set a O(N log N)
sizeS :: Set a -> Int O(1)

PriorityQueue, siendo M la cantidad de elementos en la estructura:
emptyPQ :: PriorityQueue a O(1)
isEmptyPQ :: PriorityQueue a -> Bool O(1)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a O(log M)
maxPQ :: PriorityQueue a -> a O(1)
deleteMaxPQ :: Ord a => PriorityQueue a -> PriorityQueue a O(log M)

Map, siendo K la cantidad de claves distintas en el map:
emptyM :: Map k v O(1)
assocM :: Ord k => k -> v -> Map k v -> Map k v O(log K)
lookupM :: Ord k => k -> Map k v -> Maybe v O(log K)
deleteM :: Ord k => k -> Map k v -> Map k v O(log K)
domM :: Map k v -> [k] O(K)
--}

fundarEscuela :: EscuelaDeMagia
--Propósito: Devuelve una escuela vacía.
--Eficiencia: O(1)
fundarEscuela = EDM emptyS emptyM emptyPQ

estaVacia :: EscuelaDeMagia -> Bool
--Propósito: Indica si la escuela está vacía.
--Eficiencia: O(1)
estaVacia (EDM _ mnm _) = isEmptyM mnm

registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
--Eficiencia: O(log M)
registrar nombre (EDM sh mnm pqm) = if noEsNothing (lookupM nombre mnm)
                                    then EDM sh mnm pqm
                                    else EDM sh (assocM nombre (crearM nombre) mnm) (insertPQ (crearM nombre) pqm)

magos :: EscuelaDeMagia -> [Nombre]
--Propósito: Devuelve los nombres de los magos registrados en la escuela.
--Eficiencia: O(M)
magos (EDM _ mnm _) = domM mnm

hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
--Propósito: Devuelve los hechizos que conoce un mago dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
hechizosDe nombre (EDM _ mnm _) = hechizos (fromJust(lookupM nombre mnm))

leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
--Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
leFaltanAprender nombre (EDM sh mnm _) = sizeS sh - sizeS (hechizosDe nombre)

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
--Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
--Precondición: Hay al menos un mago.
--Eficiencia: O(log M)
egresarUno (EDM sh mnm pqm) = let m = maxPQ pqm
                              in (m, EDM sh (deleteM (nombre m) mnm), (deleteMaxPQ pqm)) 

enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Enseña un hechizo a un mago existente, y 
--si el hechizo no existe en la escuela es incorporado a la misma.
--Nota: No importa si el mago ya conoce el hechizo dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(M log M + log H)
enseñar hechizo nombre (EDM sh mnm pqm) = let m = aprender hechizo (fromJust(lookupM nombre mnm)
                                          in EDM (addS hechizo sh) (assocM nombre m mnm) (modificarPQ m pq)

modificarPQ :: Mago -> PriorityQueue -> PriorityQueue
-- Propósito - Modifica dentro de la PQ dada el mago que tenga el mismo nombre por el mago dado.
-- Precondición: Hay un mago con el mismo nombre que el mago dado en la PQ.
modificarPQ mago pqm = if mago == maxPQ pqm
                      then insertPQ mago (deleteMaxPQ pqm)
                      else insertPQ (maxPQ pqm) (modificarPQ mago (deleteMaxPQ pqm))

--------------    Usuario     ---------------
--Implementar las siguientes funciones como usuario del tipo EscuelaDeMagia:

hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
--Propósito: Retorna todos los hechizos aprendidos por los magos.
--Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos edm = hechizosDeEn (magos edm) edm

hechizosDeEn :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
hechizosAprendidos [] _ = emptyS
hechizosAprendidos (m:ms) edm = unionS (hechizosDe m edm) (hechizosDeEn ms edm)

hayUnExperto :: EscuelaDeMagia -> Bool
--Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
--Eficiencia: O(log M)
hayUnExperto edm = not (estaVacia edm) && hayUnMagoYEsExperto edm

hayUnMagoYEsExperto :: EscuelaDeMagia -> Bool
hayUnMagoYEsExperto edm = let m = fst egresarUno (edm)
                          in leFaltanAprender (nombre m) edm == 0

egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
--Propósito: Devuelve un par con la lista de magos que saben todos los 
--hechizos dados por la escuela y la escuela sin dichos magos.
--Eficiencia: O(M log M)
egresarExpertos edm = if not (hayUnExperto edm)
                      then ([], edm)
                      else let (m, escuelaSinM) = egresarUno edm
                               (ms, escuelaSinMS) = egresarExpertos escuelaSinM
                      in (m:ms), escuelaSinMS

-- BONUS --
data Mago = M Nombre (Set Hechizo)
