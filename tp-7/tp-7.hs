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

arbol3 :: Tree Int
arbol3 =
  NodeT 1
    (NodeT 2
      (NodeT 3 EmptyT EmptyT)
      (NodeT 4 EmptyT EmptyT))
    (NodeT 5
      (NodeT 6 EmptyT EmptyT)
      (NodeT 7 EmptyT EmptyT))

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

-- Ejercicio 3
-- Dada la siguiente interfaz y costos para el tipo abstracto Map:

--emptyM :: Map k v
--Costo: O(1).

--assocM :: Ord k => k -> v -> Map k v -> Map k v
--Costo: O(log K).

--lookupM :: Ord k => k -> Map k v -> Maybe v
--Costo: O(log K).

--deleteM :: Ord k => k -> Map k v -> Map k v
--Costo: O(log K).

--keys :: Map k v -> [k]
--Costo: O(K).

-- Recalcular el costo de las funciones como usuario de Map de la práctica anterior, siendo K es la
-- cantidad de claves del Map. Justificar las respuestas.

{-
valuesM O(K log K)

listToMap O(K log K)

mapToList O(K log K)

agruparEq O(n^2)

incrementar O(n^2)

mergeMaps O(n^2)

indexar O(n log K)
-}

-- Dado la siguiente representación para el tipo abstracto Empresa:

type SectorId = Int

type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

{--Dicho esto, indicar invariantes de representación adecuados para la estructura y definir la
siguiente interfaz de Empresa, respetando los costos dados y calculando los faltantes. Justificar
todos los costos dados. En los costos, S es la cantidad de sectores de la empresa, y E es la
cantidad de empleados.--}

Interfaz Empresa
{-INV. REP.: 
            Sea ConsE mss mce
            * Todos los empleados de mce tienen asociado un solo CUIL.
            * El CUIL del empleado debe ser el mismo con el que está asociado en mce.
            * Todos los empleados que trabajan en algún sector de la empresa (mss),
            tienen un cuil asociado en la misma (en mce).
            * Todos los empleados que trabajan en algún sector (empleados de mce 
            que se encuentran en mss), deben tener el mismo incorporado.
-}

consEmpresa :: Empresa
--Propósito: construye una empresa vacía.
--Costo: O(1)
consEmpresa = ConsE emptyM emptyM

buscarPorCUIL :: CUIL -> Empresa -> Empleado
--Propósito: devuelve el empleado con dicho CUIL.
--Costo: O(log E) - Porque el costo de lookupM implementado con árboles es O(log e), siendo 'e' 
--la cantidad de cuils/empleados 
--Precondición: El empleado existe en la empresa dada.
buscarPorCUIL cuil (ConsE _ map) = fromJust(lookupM cuil map)

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--Propósito: indica los empleados que trabajan en un sector dado.
--Costo: O(logS + E) - Porque el costo de lookupM con arboles es logS (sectores) y setToList es constante. DUDA -- setToList es constante o lineal? A que se refieren con 'E'?
---Precondición: El sector existe en la empresa dada.
buscarPorCUIL cuil (ConsE _ map) = lookupM cuil map
empleadosDelSector sector (ConsE map _) = setToList (fromJust(lookupM sector map))

todosLosCUIL :: Empresa -> [CUIL]
--Propósito: indica todos los CUIL de empleados de la empresa.
--Costo: O(E)
todosLosCUIL (ConsE _ map) = keys map

todosLosSectores :: Empresa -> [SectorId]
--Propósito: indica todos los sectores de la empresa.
--Costo: O(S)
todosLosSectores (ConsE map _) = keys map

agregarSector :: SectorId -> Empresa -> Empresa
--Propósito: agrega un sector a la empresa, inicialmente sin empleados.
--Costo: O(logS) ya que assocM es log K.
agregarSector sector (ConsA map map2) = ConsA (assocM sector emptySet map) map2

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
--Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá
--el CUIL dado.
--Costo: calcular.
agregarEmpleado [] cuil emp = agregarEmpSinSectores (consEmpleado cuil) emp
agregarEmpleado sectores cuil emp = let empleadoConSectoresIncorporados = incorporarSectores (consEmpleado cuil) sectores 
                                    in ConsE (agregarEmpleadoASectores empleadoConSectoresIncorporados   sectores  (mapDeSectores emp)) 
                                             (agregarEmpleadoM  empleadoConSectoresIncorporados   (mapDeEmpleados emp))

agregarEmpSinSectores :: Empleado -> Empresa -> Empresa
agregarEmpSinSectores empleado (ConsE _ map) = assocM (cuil empleado) empleado map

agregarEmpleadoASectores :: Empleado -> [SectorID] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmpleadoASectores _ [] map       = map
agregarEmpleadoASectores emp (s:ss) map = if elem s (keys map)
                                          then assocM s (addS emp (fromJust(lookupM s map))) (agregarEmpleadoASectores emp ss map) 
                                          else assocM s (addS emp emptyS) (agregarEmpleadoASectores emp ss map) 

agregarEmpleadoM :: Empleado -> Map CUIL Empleado -> Map CUIL Empleado
agregarEmpleadoM emp map = assocM (cuil emp) emp map

incorporarSectores :: Empleado -> [SectorID] -> Empleado
incorporarSectores emp []     = emp
incorporarSectores emp (s:ss) = incorporarSector s (incorporarSectores emp ss)

mapDeEmpleados :: Empresa -> Map CUIL Empleado
mapDeEmpleados (ConsE _ map) = map

mapDeSectores :: Empresa -> Map SectorId (Set Empleado)
mapDeSectores (ConsE map _) = map

Recordatorio: "data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)"

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
--Propósito: agrega un sector al empleado con dicho CUIL.
--Costo: calcular.
agregarASector sector cuil (ConsE map1 map2) = let empleadoConSectorIncorporado = incorporarSector sector (consEmpleado cuil) -- Tengo q construirlo o hacerle lookup porque ese empleado ya existe?
                                               in ConsE (agregarEmpleadoASector sector empleadoConSectorIncorporado map1) 
                                                        (agregarEmpleadoM empleadoConSectorIncorporado map2) -- DUDA: Este empleado tengo q agregarlo o ya existe? - TENGO QUE AGREGARLO
                                                        -- PORQUE SINO, CUANDO LE PREGUNTE LOS SECTORES AL EMPLEADO DE LA EMPRESA, NO LOS VA A TENER INCORPORADOS.

agregarEmpleadoASector :: SectorID -> Empleado -> Map SectorID (Set Empleado) -> Map SectorID (Set Empleado)
agregarEmpleadoASector sector emp map = if elem sector (keys map)
                                        then assocM s (addS emp (fromJust(lookupM s map))) map
                                        else assocM s (addS emp emptyS) map

borrarEmpleado :: CUIL -> Empresa -> Empresa
--Propósito: elimina al empleado que posee dicho CUIL.
--Costo: calcular.
borrarEmpleado cuil (ConsE map1 map2) = ConsE (borrarEmpleadoDeSectores (sectores cuil) (fromJust(lookupM cuil map2)) map1) 
                                              (deleteM cuil (mapDeEmpleados map2))

borrarEmpleadoDeSectores :: [SectorID] -> Empleado -> Map SectorID (Set Empleado) -> Map SectorID (Set Empleado)
borrarEmpleadoDeSectores [] emp map = map
borrarEmpleadoDeSectores (s:ss) emp map = assocM s (removeS emp (fromJust(lookupM s (borrarEmpleadoDeSectores ss emp map)))) map







































