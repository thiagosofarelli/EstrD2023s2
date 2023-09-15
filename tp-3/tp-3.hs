-- Tipos algebraicos -> Se definen a través de CONSTRUCTORES (formas 
-- de hablar de los datos que yo puedo definir por mí mismo).
-- Se acceden mediante PATTERN MATCHING.
-- Se clasifican en:
-- Enumerativos (ej: Dirección(varios constructores sin argumentos)).
-- Registros (ej: Persona (único constructor - varios argumentos)).
-- Sumas o Variantes (ej: Helado (varios constructores con argumentos)).
-- Resursivos (ej: Listas (suma que usa el mismo tipo como argumento)).

-- TIPOS ALGEBRAICOS RECURSIVOS LINEALES

-- Ejercicio 1. Tipos recursivos simples

-- Ejercicio 1.1

data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia       = 0
nroBolitas c (Bolita col cel) = unoSi (sonElMismoColor c col) + nroBolitas c cel

poner :: Color -> Celda -> Celda
poner col cel = Bolita col cel

sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia       = CeldaVacia
sacar c (Bolita col cel) = if sonElMismoColor c col     
                           then cel
                           else Bolita col (sacar c cel)

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ cel   = cel 
ponerN n col cel = (Bolita col (ponerN (n-1) col cel))

celda0 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda1 = Bolita Azul (Bolita Azul CeldaVacia)

sonElMismoColor :: Color -> Color -> Bool
sonElMismoColor Azul Azul = True
sonElMismoColor Rojo Rojo = True
sonElMismoColor _ _       = False

unoSi :: Bool -> Int 
unoSi True = 1 
unoSi _    = 0

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

-- Ejercicio 1.2

hayTesoro :: Camino -> Bool
-- Indica si hay un cofre en el camino.
hayTesoro Fin                 = False
hayTesoro (Nada cam)          = hayTesoro cam
hayTesoro (Cofre objetos cam) = hayTesoroEn' objetos || hayTesoro cam

camino1 = Cofre [Tesoro] (Cofre [Cacharro] (Cofre [Cacharro, Tesoro] Fin))

hayTesoroEn' :: [Objeto] -> Bool
hayTesoroEn' []     = False
hayTesoroEn' (o:os) = esTesoro o || hayTesoroEn' os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

pasosHastaTesoro :: Camino -> Int
--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
--Precondición: tiene que haber al menos un tesoro
pasosHastaTesoro Fin                 = 0
pasosHastaTesoro (Nada cam)          = 1 + pasosHastaTesoro cam
pasosHastaTesoro (Cofre objetos cam) = if hayTesoroEn' objetos
                                       then 0 
                                       else 1 + pasosHastaTesoro cam

hayTesoroEn :: Int -> Camino -> Bool
--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn 0 cam                    = hayTesoroAca cam
hayTesoroEn n Fin                    = False
hayTesoroEn n (Nada cam)             = hayTesoroEn (n-1) cam
hayTesoroEn n (Cofre objeto cam)     = hayTesoroEn (n-1) cam

hayTesoroAca :: Camino -> Bool
hayTesoroAca (Cofre objetos cam) = hayTesoroEn' objetos
hayTesoroAca _                   = False

alMenosNTesoros :: Int -> Camino -> Bool
--Indica si hay al menos "n" tesoros en el camino.
alMenosNTesoros n cam = n <= cantTesorosEn cam

cantTesorosEn :: Camino -> Int
cantTesorosEn Fin                 = 0
cantTesorosEn (Nada cam)          = 0 + cantTesorosEn cam
cantTesorosEn (Cofre objetos cam) = cantTesorosEnObj objetos + cantTesorosEn cam

cantTesorosEnObj :: [Objeto] -> Int
cantTesorosEnObj []     = 0
cantTesorosEnObj (o:os) = unoSi (esTesoro o) + cantTesorosEnObj os

cantTesorosEntre :: Int -> Int -> Camino -> Int
--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre _ _ Fin                           = 0 
cantTesorosEntre pasos1 pasos2 (Nada cam)          = cantTesorosEntre (pasos1 - 1) (pasos2 - 1) cam
cantTesorosEntre pasos1 pasos2 (Cofre objetos cam) = if pasos1 <= 0 && pasos2 >= 0
                                                     then cantTesorosEnObj objetos + cantTesorosEntre (pasos1 - 1) (pasos2 - 1) cam
                                                     else cantTesorosEntre (pasos1 - 1) (pasos2 -1) cam

-- Ejercicio 2. Tipos Arbóreos

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

arbol1 :: Tree Int
arbol1 = NodeT 10
            (NodeT 4 
                (NodeT 2 EmptyT EmptyT) 
                (NodeT 5 
                    (NodeT 6 EmptyT EmptyT) EmptyT)) 
            (NodeT 6 EmptyT EmptyT)

arbol2 :: Tree Int
arbol2 = EmptyT

-- Ejercicio 2.1

sumarT :: Tree Int -> Int
--Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT          = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
--Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
--en inglés).
sizeT EmptyT          = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2

mapDobleT :: Tree Int -> Tree Int
--Dado un árbol de enteros devuelve un árbol con el doble de cada número
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n*2) (mapDobleT t1) (mapDobleT t2)

perteneceT :: Eq a => a -> Tree a -> Bool
--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
--árbol.
perteneceT _ EmptyT          = False
perteneceT x (NodeT n t1 t2) = x == n || perteneceT x t1 || perteneceT x t2

aparicionesT :: Eq a => a -> Tree a -> Int
--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
--iguales a e.
aparicionesT _ EmptyT          = 0
aparicionesT e (NodeT n t1 t2) = unoSi (e == n) + aparicionesT e t1 + aparicionesT e t2

leaves :: Tree a -> [a]
--Dado un árbol devuelve los elementos que se encuentran en sus hojas
leaves EmptyT                  = []
leaves (NodeT a EmptyT EmptyT) = [a]
leaves (NodeT a t1 t2)         = (leaves t1) ++ (leaves t2)

heightT :: Tree a -> Int
--Dado un árbol devuelve su altura.
--Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
--de niveles del árbol1
--La altura para EmptyT es 0, y para una hoja es 1
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

mirrorT :: Tree a -> Tree a
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol.
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

toList :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
--y luego los elementos del hijo derecho.
toList EmptyT          =  []
toList (NodeT x t1 t2) = (toList t1) ++ [x] ++ (toList t2)

levelN :: Int -> Tree a -> [a]
--Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
--nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
--distancia de la raiz a uno de sus hijos es 1.
--Nota: El primer nivel de un árbol (su raíz) es 0.
levelN _ EmptyT          = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2 

listPerLevel :: Tree a -> [[a]]
--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
--dicho árbol.
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNivel (listPerLevel t1) (listPerLevel t2)

juntarNivel :: [[a]] -> [[a]] -> [[a]]
juntarNivel []  ys         = ys
juntarNivel xs  []         = xs
juntarNivel (x:xs)  (y:ys) = (x ++ y) : juntarNivel xs ys

ramaMasLarga :: Tree a -> [a]
--Devuelve los elementos de la rama más larga del árbol.
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = if heightT t1 > heightT t2
                               then x : ramaMasLarga t1
                               else x : ramaMasLarga t2

todosLosCaminos :: Tree a -> [[a]]
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera
--de los nodos.
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT e t1 t2) = [e] : consACada e (todosLosCaminos t1) ++ consACada e (todosLosCaminos t2)

consACada :: a -> [[a]] -> [[a]]
consACada n []       = []
consACada n (x:xs)   = (n:x) : consACada n xs

-- 2.2 - Expresiones Aritméticas

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
    deriving Show

exp1 = Sum(Neg (Valor (-3)))(Prod(Sum(Valor 0)(Valor 6))(Valor 10))

eval :: ExpA -> Int
--Dada una expresión aritmética devuelve el resultado evaluarla.
eval (Valor n)    = n
eval (Sum v1 v2)  = eval v1 + eval v2
eval (Prod v1 v2) = eval v1 * eval v2
eval (Neg v1)     = eval v1 * (-1)

simplificar :: ExpA -> ExpA
--Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
--notación matemática convencional):
--a) 0 + x = x + 0 = x
--b) 0 * x = x * 0 = 0
--c) 1 * x = x * 1 = x
--d) - (- x) = x
simplificar (Valor n)    = Valor n
simplificar (Sum v1 v2)  = simplificarSum  (simplificar v1) (simplificar v2)
simplificar (Prod v1 v2) = simplificarProd (simplificar v1) (simplificar v2)
simplificar (Neg v1)     = simplificarNeg  (simplificar v1)

simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum (Valor 0) v2 = v2
simplificarSum v1 (Valor 0) = v1
simplificarSum v1 v2        = Sum v1 v2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) v2 = Valor 0
simplificarProd v1 (Valor 0) = Valor 0
simplificarProd (Valor 1) v2 = v2
simplificarProd v1 (Valor 1) = v1
simplificarProd v1 v2        = Prod v1 v2

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg e)  = e
simplificarNeg v1        = (Neg v1)

