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

camino1 = Cofre [Cacharro] (Cofre [Cacharro] (Cofre [Cacharro, Tesoro] Fin))

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