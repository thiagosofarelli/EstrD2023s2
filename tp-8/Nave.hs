{--
CONSTANTE       - Siempre el mismo costo                            O(1)
LOGARÍTMICO     - Búsqueda en un árbol                              O(log n)
LINEAL          - Solo operaciones constantes por elemento          O(n)
ENELOGUENE      - Hasta operaciones logarítmicas por elemento       O(n log n)
CUADRÁTICA      - Hasta operaciones lineales por elemento           O(n^2)

TADs a conocer: Stacks, Queues, Priority Queues, Maps, Multisets.
Implementaciones: BSTs, AVLs, Heaps.
--}

-- NAVE -- 

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)


{-- Inv. Rep.:
- Sea N ms mt ht: 
- Los sectores de 'ms' tienen que tener un id igual al SectorID con el que estan asociados en 'ms'.
- El nombre de cada Tripulante de 'mt' es el mismo con el que está asociado en 'mt'.
- Todos los Tripulantes de 'mt' están en 'ht' y viceversa.
- Todos los sectores en los que trabajan los tripulantes se encuentran en 'ms', y todos los tripulantes de los sectores de 'ms' se encuentran en 'mt'.
- Los sectores y el rango del Tripulante con nombre 'x' en mt, son los mismos sectores y rango del Tripulante con ese nombre en 'ht'.
- En 'ht' no hay Tripulantes con el mismo nombre.
--}

{--
Sector, siendo C la cantidad de contenedores y T la cantidad de tripulantes:
crearS :: SectorId -> Sector O(1)
sectorId :: Sector -> SectorId O(1)
componentesS :: Sector -> [Componente] O(1)
tripulantesS :: Sector -> Set Nombre O(1)
agregarC :: Componente -> Sector -> Sector O(1)
agregarT :: Nombre -> Sector -> Sector O(log T)


Tripulante, siendo S la cantidad de sectores:
crearT :: Nombre -> Rango -> Tripulante O(1)
asignarS :: SectorId -> Tripulante -> Tripulante
O(log S)
sectoresT :: Tripulante -> Set SectorId O(1)
nombre :: Tripulante -> String O(1)
rango :: Tripulante -> Rango O(1)


Set, siendo N la cantidad de elementos del conjunto:
emptyS :: Set a O(1)
addS :: a -> Set a -> Set a O(log N)
belongsS :: a -> Set a -> Bool O(log N)
unionS :: Set a -> Set a -> Set a O(N log N)
setToList :: Set a -> [a] O(N)
sizeS :: Set a -> Int O(1)


MaxHeap, siendo M la cantidad de elementos en la heap:
emptyH :: MaxHeap a O(1)
isEmptyH :: MaxHeap a -> Bool O(1)
insertH :: a -> MaxHeap a -> MaxHeap a O(log M)
maxH :: MaxHeap a -> a O(1)
deleteMaxH :: MaxHeap a -> MaxHeap a O(log M)


Map, siendo K la cantidad de claves distintas en el map:
emptyM :: Map k v O(1)
assocM :: k -> v -> Map k v -> Map k v O(log K)
lookupM :: k -> Map k v -> Maybe v O(log K)
deleteM :: k -> Map k v -> Map k v O(log K)
domM :: Map k v -> [k] O(K)
--}

-- NAVE -- 

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
-- Sea N ms mt ht --


--Implementación
--Implementar la siguiente interfaz de Nave, utilizando la representación y los costos dados, calculando los costos de cada
--subtarea, y siendo T la cantidad de tripulantes y S la cantidad de sectores:

construir :: [SectorId] -> Nave
--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
--Eficiencia: O(S log K) ya que construir sectores posee dicho costo.
construir [] = N emptyM emptyM emptyH
construir sectores = N (construirSectores sectores emptyM) emptyM emptyH

construirSectores :: [SectorId] -> Map SectorId Sector -> Map SectorId Sector
construirSectores [] ms = ms
construirSectores (s:ss) ms = assocM s (crearS s) (construirSectores ss ms)
{--  Eficiencia: O(S) * O(log K) + O(1) = O(S log K)
- Justificación: Esta función tiene el costo O(S log K) ya que por cada S (sector), realiza una
operación logarítmica mediante el assocM.
    * assocM tiene costo O(log K) siendo K los distintos sectores asociados a 'ms'.  
    * crearS tiene costo O(1).
--}

ingresarT :: Nombre -> Rango -> Nave -> Nave
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
ingresarT nombre rango (N ms mt ht) = let t = crearT nombre rango
                                      in N ms (assocM nombre t mt) (insertH t ht)
{--  Eficiencia: O(log K) + O(log M) + O(1) = O(log K + log M)
- Justificación: Esta función tiene el costo O(log K + log M) donde K son los distintos
tripulantes asociados a 'mt' y M son la cantidad de elementos en 'ht'.
    * crearT tiene costo constante.
    * assocM tiene costo O(log K) siendo K los distintos tripulantes asociados a 'mt'.
    * insertH tiene costo O(log M) siendo M la cantidad de elementos en 'ht'.
--}

sectoresAsignados :: Nombre -> Nave -> Set SectorId
--Propósito: Devuelve los sectores asignados a un tripulante.
--Precondición: Existe un tripulante con dicho nombre.
sectoresAsignados nombre (N _ mt _) = sectoresT fromJust(lookupM nombre mt)
{--  Eficiencia: O(log K) + O(1) + O(1) = O(log K)
- Justificación: Esta función tiene el costo O(log K) siendo K los tripulantes asociados a 'mt'.
    * sectoresT tiene costo O(1).
    * fromJust tiene costo O(1).
    * lookupM tiene costo O(log K) siendo K los distintos tripulantes asociados en 'mt'. 
--}

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
datosDeSector sectorid (N ms mt ht) = let sector = fromJust(lookupM sectorid ms)
                                      in  (tripulantesS sector, componentesS sector)
{--  Eficiencia: O(log S) + O(1) + O(1) + O(1) = O(log K)
Justificación: Esta función tiene el costo O(log K) siendo K la cantidad de sectores en 'ms'.
    * tripulantesS tiene costo O(1).
    * componentesS tiene costo O(1).
    * fromJust tiene costo O(1).
    * lookupM tiene costo O(log K) siendo K la cantidad de sectores en 'ms'. 
--}

tripulantesN :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
{--  Eficiencia: O(T log M) ya que la función listaTripulantes posee dicho costo.
--}
tripulantesN (N _ _ ht) = listaTripulantes ht

listaTripulantes :: MaxHeap Tripulante -> [Tripulante]
listaTripulantes ht = if isEmptyH ht
                      then []
                      else maxH ht : listaTripulantes (deleteMaxH ht)
{--  Eficiencia: O(T) * (O(1) + O(1) + O(log M)) = O(T log M)
Justificación: Esta función tiene el costo O(T log M) ya que por cada (T) tripulante de la heap,
realiza una operación logarítmica.
    * isEmptyH tiene costo constante.
    * maxH tiene costo constante.
    * deleteMaxH tiene costo O(log M) siendo M la cantidad de elementos de 'ht'. 
--}

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Asigna una lista de componentes a un sector de la nave.
agregarASector [] _ n = n
agregarASector componentes sectorid (N ms mt ht) = let sectorConComponentes = agregarComponentes componentes (fromJust(lookupM sectorid ms))
                                                   in N (assocM sectorid sectorConComponentes ms) mt ht 
{--  Eficiencia: O(C) + O(log K) + O(log K) = O(C + log K) 
Justificación: Esta función tiene el costo O(C + log K) siendo K los sectores del map, y C la operación lineal a realizar.
-- DUDA: Hay que explicar lo que HACE cada función diciendo 'ya que...' o directamente hay que poner 'siendo 'x' la cantidad de ...'?
    * agregarC tiene costo O(C)
    * fromJust tiene costo constante.
    * lookupM tiene costo O(log K) siendo K la cantidad de sectores en 'ms'.
    * assocM  tiene costo O(log K) siendo K la cantidad de sectores en 'ms'
--}

agregarComponentes :: [Componente] -> Sector -> Sector
agregarComponentes [] sector = sector
agregarComponentes (c:cs) sector = agregarC c (agregarComponentes cs sector)
{--  Eficiencia: O(C)  
Justificación: Esta función tiene el costo O(C) siendo C la cantidad de componentes a agregar. 
    * agregarC tiene costo O(1).
--}


asignarASector :: Nombre -> SectorId -> Nave -> Nave
--Propósito: Asigna un sector a un tripulante.
--Nota: No importa si el tripulante ya tiene asignado dicho sector.
--Precondición: El tripulante y el sector existen.
asignarASector nombre sectorid (N ms mt ht) = let t = asignarS sectorid (fromJust(lookupM nombre mt))
                                              in N (asignarASectorEnSectores sectorid nombre ms ) (asignarTripAMapT nombre t mt) (modificarHeap t ht)
{--  Eficiencia: O(log K + log S + H log M) 
Justificación: Esta función tiene el costo O 
    * asignarS - O(log S) - Siendo S sectores
    * fromJust - O(1)
    * lookupM - O(log K) - Siendo K tripulantes
    * asignarASectorEnSectores - O(log K + log T) siendo K sectores y T el tripulante.
    * asignarTripAMapT - O(log K) siendo K los distintos tripulantes.
    * modificarHeap - O(H log M) ya que por cada Heap realizo hasta operaciones logaritmicas M. 
--}

asignarASectorEnSectores :: SectorID -> nombre -> Map SectorID Sector -> Map SectorID Sector
asignarASectorEnSectores sectorid nombre ms = let sector = fromJust(lookupM sectorid ms)
                                              in assocM sectorid (agregarT nombre sector) ms
{--  Eficiencia: O(log K)+ O(log K) + O(log T) = O(log K + log T)
Justificación: Esta función tiene el costo O(log K + log T) siendo K los distintos sectores de 'ms' y T el tripulante a asignar.
    * lookupM tiene costo O(log K) - Siendo K los sectores de 'ms'.
    * fromJust tiene costo constante.
    * assocM tiene costo O(log K) - Siendo K los sectores de 'ms'.
    * agregarT tiene costo O(log T) - (DUDA - Es 'T' el tripulante a asignar?)
--}

asignarTripAMapT :: Nombre -> Tripulante -> Map Nombre Tripulante -> Map Nombre Tripulante
asignarTripAMapT nombre trip mt = assocM nombre trip mt
{--  Eficiencia: O(log K) 
Justificación: La función tiene costo O(log K) ya que utiliza la función assocM con dicho costo.
* assocM tiene costo O(log K) siendo K los distintos tripulantes de 'mt'.
--}

modificarHeap :: Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante
-- Propósito - Modifica dentro de la Heap dada el tripulante que tenga el mismo nombre por el tripulante dado.
-- Precondición: Hay un tripulante con el mismo nombre que el tripulante dado en la Heap.
modificarHeap trip heap = if trip == maxH heap
                          then insertH trip (deleteMaxH heap)
                          else insertH (maxH heap) (modificarHeap trip (deleteMaxH Heap))
{--  Eficiencia: O(H) * O(log M + log M + 1)  = (H log M)
Justificación: La función tiene costo O(H log M) ya que por cada H (HEAP) realizo operaciones logarítmicas. [DUDA: Esta bien justificado?]
    * insertH tiene costo O(log M) - Siendo M la cantidad de elementos en la heap.
    * maxH tiene costo constante.
    * deleteMaxH tiene costo O(log M) - Siendo M la cantidad de elementos en la heap.
--}


--Usuario
--Implementar las siguientes funciones como usuario del tipo Nave, indicando la eficiencia obtenida para cada operación:

sectores :: Nave -> Set SectorId
--Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores nave = sectoresAsignadosA (tripulantesN nave) nave
{--  Eficiencia: O(S^2 + T log M) = O(S^2 + T log M)
    * sectoresAsignadosA - O(S^2)
    * tripulantesN - O(T log M) 
--}


sectoresAsignadosA :: [Tripulante] -> Nave -> Set SectorID
sectoresAsignadosA [] nave = emptyS
sectoresAsignadosA (t:ts) nave = unionS (sectoresAsignados (nombre t) nave) (sectoresAsignadosA ts nave)
{--  Eficiencia: O(SET) * (O S log S + log K) = O(SET^2) 
Justificación: Se realizan hasta operaciones lineales por cada set. -- DUDA
    * unionS - O(S log S) siendo S la cantidad de sectores
    * sectoresAsignados - O(log K) siendo K los Tripulantes.
    * nombre - O(1)
--}

sinSectoresAsignados :: Nave -> [Tripulante]
--Propósito: Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados nave = sinSectoresAsignadosT (tripulantesN n)
{- Eficiencia: O(T^N) - DUDA: log T no se utiliza ya que el peor caso es T^N? y son del mismo tipo?
* sinSectoresAsignadosT - O(T^N)
* TripulantesN - log T siendo T la cantidad de Tripulante
-}

sinSectoresAsignadosT :: [Tripulante] -> [Tripulante]
sinSectoresAsignadosT [] = []
sinSectoresAsignadosT (t:ts) = if noPoseeSector t
                               then t : sinSectoresAsignadosT ts
                               else sinSectoresAsignadosT ts
{- Eficiencia: O(T^N) siendo T la cantida de tripulantes, y N los sectores asignados a cada tripulante) 
-- DUDA: Esto es cuadrático porque a cada elemento de la lista [Tripulante] le hago una operación de costo lineal?
* noPoseeSector - costo O(N)
-}


noPoseeSector :: Tripulante -> Bool
noPoseeSector t = null (setToList (sectoresT t))
{- Eficiencia: O(N)
* setToList - costo O(N) siendo N la cantidad de elementos del conjunto.
* sectoresT - costo constante
* null      - costo constante
-}

barriles :: Nave -> [Barril]
--Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles nave = barrilesS (sectores nave) nave
-- sectores nave tiene costo O(S log S + log T)
-- barrilesS tiene costo O(S)
-- Costo final sería O(S log S + log T) y se cancela O(S)? [DUDA]

barrilesS :: Set SectorID -> Nave -> [Barril]
barrilesS set nave = let sectoresID = setToList set 
                     in barrilesDeCadaSector sectoresID nave
-- Eficiencia O(S) ya que se usa la función barrilesDeCadaSector
-- También se usa setToList que es costo O(N). En este caso, como quedaría la eficiencia final? [DUDA]

barrilesDeCadaSector :: [SectorID] -> Nave -> [Barril]
barrilesDeCadaSector [] _ = []
barrilesDeCadaSector (s:ss) nave = soloBarriles (snd (datosDeSector s nave)) ++ (barrilesDeCadaSector ss nave)
-- * SoloBarriles - O(S)
-- * SND - O(1)
-- * datosDeSector (log S) pero por cada sector hago un log S, asi que es costo S log S
-- Eficiencia: O(S) y se cancela log S. DUDA: Esto es correcto? Debería ser cuadrática porque por cada barril, obtengo los sectores.

soloBarriles :: [Componente] -> [Barril]
soloBarriles [] = []
soloBarriles (c:cs) if esAlmacen c
                    then barrilesDeAlmacen c ++ soloBarriles cs
                    else soloBarriles cs
-- * esAlmacen - Constante
-- * Eficiencia O(N) Lineal, por cada Almacen hago un append
-- DUDA: Esta bien justificado?

esAlmacen :: Componente -> Bool
esAlmacen (Almacen _) = True
esAlmacen _ = False
-- * CONSTANTE -- DUDA: Hace falta justficar?

barrilesDeAlmacen :: Componente -> [Barril]
barrilDelAlmacen (Almacen barriles) = barriles
-- Precondicion: el componente dado es un almacen.
-- * CONSTANTE -- DUDA: Hace falta justficar?

-----   Bonus  -----
--Dar una posible representación para el tipo Sector, de manera de que se pueda cumplir con el orden dado para cada
--operación de la interfaz, pero sin implementarlas.
data Sector = S Nombre [Componente] (Set Nombre)
