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

celda0 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda1 = Bolita Azul (Bolita Azul CeldaVacia)

sonElMismoColor :: Color -> Color -> Bool
sonElMismoColor Azul Azul = True
sonElMismoColor Rojo Rojo = True
sonElMismoColor _ _ = False

unoSiOCeroSiNo :: Bool -> Int 
unoSiOCeroSiNo True = 1 
unoSiOCeroSiNo _ = 0
