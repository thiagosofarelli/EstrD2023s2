-- Tipos algebraicos -> Se definen a través de CONSTRUCTORES (formas 
-- de hablar de los datos que yo puedo definir por mí mismo).
-- Se acceden mediante PATTERN MATCHING.
-- Se clasifincan en:
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
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita col cel) = unoSiOCeroSiNo (sonElMismoColor c col) + nroBolitas c cel

poner :: Color -> Celda -> Celda
poner col cel = Bolita col cel

sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar c (Bolita col cel) = if sonElMismoColor c col     
                           then cel
                           else (Bolita col (sacar c cel)) 

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ cel = cel 
ponerN n col cel = (Bolita col (ponerN (n-1) col cel))

celda0 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda1 = Bolita Azul (Bolita Azul CeldaVacia)

sonElMismoColor :: Color -> Color -> Bool
sonElMismoColor Azul Azul = True
sonElMismoColor Rojo Rojo = True
sonElMismoColor _ _ = False

unoSiOCeroSiNo :: Bool -> Int 
unoSiOCeroSiNo True = 1 
unoSiOCeroSiNo _ = 0

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

-- Ejercicio 1.2

hayTesoro :: Camino -> Bool
-- Indica si hay un cofre en el camino.
hayTesoro Fin = False
hayTesoro (Nada cam) = hayTesoro cam
hayTesoro (Cofre objetos cam) = hayTesoroEn' objetos || hayTesoro cam

camino1 = Cofre [Tesoro] (Cofre [Cacharro] (Cofre [Cacharro, Tesoro] Fin))

hayTesoroEn' :: [Objeto] -> Bool
hayTesoroEn' [] = False
hayTesoroEn' (o:os) = esTesoro o || hayTesoroEn' os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

pasosHastaTesoro :: Camino -> Int
--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
--Precondición: tiene que haber al menos un tesoro
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Nada cam) = 1 + pasosHastaTesoro cam
pasosHastaTesoro (Cofre objetos cam) = if hayTesoroEn' objetos
                                       then 0 
                                       else 1 + pasosHastaTesoro cam

hayTesoroEn :: Int -> Camino -> Bool
--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn n cam = pasosHastaTesoro cam == n  

alMenosNTesoros :: Int -> Camino -> Bool
--Indica si hay al menos "n" tesoros en el camino.
alMenosNTesoros n cam = n <= cantTesorosEn cam

cantTesorosEn :: Camino -> Int
cantTesorosEn Fin = 0
cantTesorosEn (Nada cam) = 0 + cantTesorosEn cam
cantTesorosEn (Cofre objetos cam) = cantTesorosEnObj objetos + cantTesorosEn cam

cantTesorosEnObj :: [Objeto] -> Int
cantTesorosEnObj [] = 0
cantTesorosEnObj (o:os) = unoSiOCeroSiNo (esTesoro o) + cantTesorosEnObj os

cantTesorosEntre :: Int -> Int -> Camino -> Int
--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre _ _ Fin = 0 
cantTesorosEntre pasos1 pasos2 (Nada cam) = cantTesorosEntre (pasos1 - 1) (pasos2 - 1) cam
cantTesorosEntre pasos1 pasos2 (Cofre objetos cam) = if pasos1 <= 0 && pasos2 >= 0
                                                     then cantTesorosEnObj objetos + cantTesorosEntre (pasos1 - 1) (pasos2 - 1) cam
                                                     else cantTesorosEntre (pasos1 - 1) (pasos2 -1) cam

