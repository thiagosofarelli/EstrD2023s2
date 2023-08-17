-- Práctica de ejercicios # 1 - Tipos algebraicos

-- Ejercicio 2.1

-- a

sucesor :: Int -> Int
sucesor n = n + 1

-- b

sumar :: Int -> Int -> Int
sumar x y = x + y

-- c

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)

-- d

maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if (n > m)
                   then n
                   else m

-- Ejercicio 2.2

-- ejemplo 1

-- sucesor (sumar 8 1)

-- ejemplo 2

-- maxDelPar (divisionYResto (sumar 10 90) (sucesor 9))

-- ejemplo 3

-- sucesor (maxDelPar (sumar 3 6, sumar 3 4))

-- ejemplo 4

-- sumar 12 (-2)

-- Ejercicio 3.1

data Dir = Norte | Sur | Este | Oeste
    deriving Show

-- a

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

-- b

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

-- c

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte

-- Si se supone que no existe la siguiente dirección a Oeste,
-- la función sería parcial ya que no funcionaría con todas las
-- variantes del tipo de dato Dir, y la precondición
-- de la función sería que no se la puede llamar con 'Oeste'.

-- Ejercicio 3.2

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

-- a

primerYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primerYUltimoDia = (primerDiaDeSemana, ultimoDiaDeSemana)

primerDiaDeSemana :: DiaDeSemana
primerDiaDeSemana = Lunes

ultimoDiaDeSemana :: DiaDeSemana
ultimoDiaDeSemana = Domingo

-- b 

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

-- c

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues Lunes Domingo = True
vieneDespues _ _ = False 

-- d

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Martes = True
estaEnElMedio Miercoles = True
estaEnElMedio Jueves = True
estaEnElMedio Viernes = True
estaEnElMedio Sabado = True
estaEnElMedio _ = False

-- Ejercicio 3.3

-- a

negar :: Bool -> Bool
negar True = False
negar False = True

-- b 

implica :: Bool -> Bool -> Bool
implica True b = b
implica _ _ = True

-- c

yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien _ _ = False

-- d

oBien :: Bool -> Bool -> Bool
oBien False b = b
oBien _ _ = True

-- Ejercicio 4.1

-- 1 

data Persona = P String Int
    deriving Show

nombre :: Persona -> String
nombre (P n _) = n

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (P n e) = P nuevoNombre e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = if (e1 > e2)
                                     then True
                                     else False

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n1 e1) (P n2 e2) = if (e1 > e2)
                                     then P n1 e1
                                     else P n2 e2
                                     
thiago = P "Thiago" 20
valentina = P "Valentina" 19

-- Ejercicio 4.2

data Pokemon = Poke Tipo Int   
              -- Tipo Energia   
     deriving Show

data Tipo = Agua | Fuego | Planta 
    deriving Show

data Entrenador = String | Pokemon 
    deriving Show
    
superaA :: Pokemon -> Pokemon -> Bool
superaA poke1 poke2 = tipoSuperaA (tipo poke1) (tipo poke2)

tipoSuperaA :: Tipo -> Tipo -> Bool
tipoSuperaA Agua Fuego = True
tipoSuperaA Fuego Planta = True
tipoSuperaA Planta Agua = True
tipoSuperaA _ _ = False

tipo :: Pokemon -> Tipo
tipo (Poke t _) = t

snorlax = Poke Agua 100
squirtle = Poke Fuego 200