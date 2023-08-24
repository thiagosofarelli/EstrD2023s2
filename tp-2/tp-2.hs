-- CONS = Agregar adelante - Toma un elemento de algún tipo y una lista del mismo tipo,
-- y describe la lista que resulta de agregar el elemento dado adelante de la lista dada.

-- NIL = Lista vacía - Es una lista de algún tipo a instanciar.

-- ": CONS" 
-- "[] NIL" 

-- 1:2:3:[] "Se lee: Agregar 1 adelante de la lista que agrega 2
-- adelante de la lista que agrega 3 adelante de la lista vacía" 
-- ó 1 cons 2 cons 3 cons Nil.


-- Ejercicio 1

-- 1

sumatoria :: [Int]  -> Int
sumatoria []        = 0
sumatoria (n:ns)    = n + sumatoria ns

-- 2

longitud :: [a] -> Int
longitud []        = 0
longitud (x:xs)    = 1 + longitud xs

-- 3

sucesores :: [Int] -> [Int]
sucesores     [] = []
sucesores (n:ns) = n + 1 : sucesores ns

-- 4 

conjuncion :: [Bool] -> Bool
conjuncion     [] = False
conjuncion (b:bs) = b && conjuncion bs

-- 5 

disyuncion :: [Bool] -> Bool
disyuncion     [] = False
disyuncion (b:bs) = b || disyuncion bs

-- 6

aplanar :: [[a]] -> [a]
aplanar     [] = []
aplanar (x:xs) = x ++ aplanar xs

-- 7 

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

-- 8

apariciones :: Eq a => a -> [a] -> Int
apariciones     _ [] = 0
apariciones e (x:xs) = (if e == x then 1 else 0) + apariciones e xs
                       
-- 9

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA     _ [] = []
losMenoresA n (x:xs) = if n > x 
                       then x : losMenoresA n xs 
                       else losMenoresA n xs

-- 10

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA     _ [] = []
lasDeLongitudMayorA n (l:ls) = if (longitud l) > n 
                               then l : lasDeLongitudMayorA  n ls 
                               else lasDeLongitudMayorA n ls

-- 11

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal     [] e = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

-- 12 

agregar :: [a] -> [a] -> [a]
agregar []     ys = ys
agregar (x:xs) ys = x : agregar xs ys

-- 13

reversa :: [a] -> [a]
reversa     [] = []
reversa (x:xs) = reversa xs ++ [x]

-- 14

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos         [] [] = []
zipMaximos         [] ys = ys
zipMaximos         xs [] = xs
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys 


-- 15

elMinimo :: Ord a => [a] -> a
elMinimo    [e] = e
elMinimo (x:xs) = min x (elMinimo xs)

-- Ejercicio 2

-- 1

factorial :: Int -> Int -- Precondición: n no es negativo.
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 2

cuentaRegresiva :: Int -> [Int] -- Precondición: n no es negativo.
cuentaRegresiva 0 = [0]
cuentaRegresiva n = n : cuentaRegresiva (n-1)

-- 3
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n x = x : repetir (n-1) x

-- 4
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _      = []
losPrimeros n []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

-- 5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs  = xs
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs


-- Ejercicio 3

-- 1

data Persona = P String Int
    deriving Show
              -- Nombre Edad

thiago = P "Thiago" 20
valentina = P "Valentina" 19
coco = P "Coco" 3
carlos = P "Carlos" 20
barrio = [thiago, valentina, coco]

edad :: Persona -> Int
edad (P _ e) = e

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (p:ps) = if n < edad p then p : mayoresA n ps else mayoresA n ps

promedioEdad :: [Persona] -> Int -- Precondición: La lista posee al menos una persona.
promedioEdad ps = promedio (edades ps)

edades :: [Persona] -> [Int]
edades [] = []
edades (p:ps) = edad p : edades ps

elMasViejo :: [Persona] -> Persona -- Precondición: La lista posee al menos una persona.
elMasViejo [p] = p
elMasViejo (p:ps) = if edad p == numeroMaximo (edades (p:ps))
                    then p
                    else elMasViejo ps 

numeroMaximo :: [Int] -> Int
numeroMaximo [] = 0
numeroMaximo (n:ns) = max n (numeroMaximo ns)

-- 2

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show
    
data Pokemon = ConsPokemon TipoDePokemon Int
    deriving Show

data Entrenador = ConsEntrenador String [Pokemon]
    deriving Show

-- Pokemones
snorlax = ConsPokemon Agua 100
cacto = ConsPokemon Agua 150
squirtle = ConsPokemon Fuego 200
cucu = ConsPokemon Planta 120
kiki = ConsPokemon Agua 300

-- Entrenadores
buchu = ConsEntrenador "Buchu" [squirtle, kiki]
cacho = ConsEntrenador "Cacho" [snorlax, cacto, cucu, kiki, squirtle]

cantPokemon :: Entrenador -> Int
--Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon e = longitud (listaDePokemones e)

listaDePokemones :: Entrenador -> [Pokemon]
listaDePokemones (ConsEntrenador _ p) = p


cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe tipo e = longitud (pokemonesDeTipoDe tipo (listaDePokemones e))

pokemonesDeTipoDe :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDeTipoDe _ [] = []
pokemonesDeTipoDe t (p:ps) = if sonDelMismoTipo t (tipo p) 
                             then p : pokemonesDeTipoDe t ps
                             else pokemonesDeTipoDe t ps

tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t _) = t

sonDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipo Agua Agua = True
sonDelMismoTipo Fuego Fuego = True
sonDelMismoTipo Planta Planta = True
sonDelMismoTipo _ _ = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int 
--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
--a los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ tipo e1 e2 = cantidadDeVictoriasDe_FrenteA (pokemonesDeTipoDe tipo (listaDePokemones e1)) (listaDePokemones e2)

cantidadDeVictoriasDe_FrenteA :: [Pokemon] -> [Pokemon] -> Int
cantidadDeVictoriasDe_FrenteA [] _ = 0
cantidadDeVictoriasDe_FrenteA (x:xs) (y:ys) = pokemonesVencidosPor_FrenteA_ x ys + cantidadDeVictoriasDe_FrenteA xs ys

pokemonesVencidosPor_FrenteA_ :: Pokemon -> [Pokemon] -> Int
pokemonesVencidosPor_FrenteA_ _ [] = 0
pokemonesVencidosPor_FrenteA_ x (y:ys) = unoSiOCeroSiNo (pokemonSuperaA x y) + pokemonesVencidosPor_FrenteA_ x ys

pokemonSuperaA :: Pokemon -> Pokemon -> Bool
pokemonSuperaA p1 p2 = tipoSuperaA (tipo p1) (tipo p2)

tipoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool 
tipoSuperaA Agua Fuego = True 
tipoSuperaA Fuego Planta = True 
tipoSuperaA Planta Agua = True 
tipoSuperaA _ _ = False

unoSiOCeroSiNo :: Bool -> Int 
unoSiOCeroSiNo True = 1 
unoSiOCeroSiNo _ = 0

-- Pokemones
--snorlax = ConsPokemon Agua 100
--cacto = ConsPokemon Agua 150
--squirtle = ConsPokemon Fuego 200

-- Entrenadores
--buchu = ConsEntrenador "Buchu" [snorlax, cacto, squirtle]
--cacho = ConsEntrenador "Cacho" [snorlax, cacto]

--esMaestroPokemon :: Entrenador -> Bool
--Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.


-- Ejercicios dados en la clase 2 (video)

hayAlMenosUnCinco :: [Int] -> Bool
hayAlMenosUnCinco [] = False
hayAlMenosUnCinco (n:ns) = n == 5 || hayAlMenosUnCinco ns

hayAlMenosUn :: Int -> [Int] -> Bool
hayAlMenosUn _ [] = False
hayAlMenosUn x (n:ns) = n == x || hayAlMenosUn x ns

soloLosMayoresQue :: Int -> [Int] -> [Int]
soloLosMayoresQue _ [] = []
soloLosMayoresQue x (n:ns)  = if n > x 
                              then n : soloLosMayoresQue x ns 
                              else soloLosMayoresQue x ns

data Dir = Norte | Sur | Este | Oeste

iniciales :: [Dir] -> [Char]
iniciales [] = []
iniciales (d:ds) = inicial d : iniciales ds

inicial :: Dir -> Char
inicial Norte = 'N'
inicial Sur   = 'S'
inicial Este  = 'E'
inicial Oeste = 'O'

miZip :: [a] -> [b] -> [(a,b)]
-- zip [10, 20, 30] ['N','S'] = [(10, 'N'), (20, 'S')]
miZip []      _     = []
miZip  _     []     = []
miZip (x:xs) (y:ys) = (x, y) : miZip xs ys

miLast :: [a] -> a -- Precondición: La lista no es vacía
miLast (x:xs) = if null xs
                then x
                else miLast xs

promedio :: [Int] -> Int -- Precondición: La lista no es vacía
promedio ns = div (sumatoria ns) (longitud ns)

miSumatoria :: Int -> Int
miSumatoria 0 = 0
miSumatoria n = n + miSumatoria (n-1)

replicar :: Int -> a -> [a] -- Precondición: El número es mayor o igual que cero.
replicar 0 x = []
replicar n x = x : replicar (n-1) x 

cuentaRegresivaDesde :: Int -> [Int]
cuentaRegresivaDesde 0 = [0]
cuentaRegresivaDesde n = n : cuentaRegresivaDesde (n-1)

losPrimerosN :: Int -> [Int] -> [Int]
losPrimerosN 0 _  = []
losPrimerosN n [] = []
losPrimerosN n (x:xs) = x : losPrimerosN (n-1) xs