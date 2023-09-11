data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

listaDePizzas = [pizza1, pizza2, pizza3, pizza4]
pizza1 = Capa Salsa (Capa Jamon (Capa Queso Prepizza))
pizza2 = Capa Queso (Capa (Aceitunas 3) Prepizza)
pizza3 = Capa Jamon (Capa Salsa Prepizza)
pizza4 = Capa (Aceitunas 4) (Capa Jamon Prepizza)

cantidadDeCapas :: Pizza -> Int
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing pizza) = 1 + cantidadDeCapas pizza

armarPizza :: [Ingrediente] -> Pizza
--Dada una lista de ingredientes construye una pizza
armarPizza []         = Prepizza
armarPizza (ing:ings) = Capa ing (armarPizza ings)

sacarJamon :: Pizza -> Pizza
--Le saca los ingredientes que sean jamón a la pizza
sacarJamon Prepizza         = Prepizza
sacarJamon (Capa ing pizza) = if esJamon ing
                              then sacarJamon pizza
                              else Capa ing (sacarJamon pizza)

esJamon :: Ingrediente -> Bool
esJamon Jamon     = True
esJamon _         = False

tieneSoloSalsaYQueso :: Pizza -> Bool
--Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
--particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing pizza) = esSalsaOQueso ing && tieneSoloSalsaYQueso pizza

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _     = False

duplicarAceitunas :: Pizza -> Pizza
--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing pizza) = if esAceituna ing
                                     then Capa (aceitunasDuplicadas ing) (duplicarAceitunas pizza)
                                     else Capa ing (duplicarAceitunas pizza)
                                     
aceitunasDuplicadas :: Ingrediente -> Ingrediente
aceitunasDuplicadas (Aceitunas n)     = Aceitunas (n*2)
aceitunasDuplicadas otroIng           = otroIng

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas n) = True
esAceituna _             = False

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantIng p, p) : cantCapasPorPizza ps 

cantIng :: Pizza -> Int
cantIng Prepizza = 0
cantIng (Capa ing p) = 1 + cantIng p

data Dir = Izq | Der
    deriving Show   

data Objeto = Tesoro | Chatarra
    deriving Show

data Cofre = Cofre [Objeto]
    deriving Show

data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

mapa1 = Bifurcacion cofre1 mapa2 mapa3
mapa2 = Fin cofre2
mapa3 = Bifurcacion cofre3 mapa2 mapa4
mapa4 = Fin cofre4
cofre1 = Cofre [Chatarra]
cofre2 = Cofre [Chatarra]
cofre3 = Cofre [Chatarra, Chatarra, Chatarra]
cofre4 = Cofre [Tesoro]

hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre) = contieneTesoro (objetosDe cofre)
hayTesoro (Bifurcacion cofre mapa1 mapa2) = contieneTesoro (objetosDe cofre) || hayTesoro mapa1 || hayTesoro mapa2

objetosDe :: Cofre -> [Objeto]
objetosDe (Cofre objetos) = objetos

contieneTesoro :: [Objeto] -> Bool
contieneTesoro [] = False
contieneTesoro (o:os) = esTesoro o || contieneTesoro os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn _ (Fin cofre) = contieneTesoro (objetosDe cofre)
hayTesoroEn [] (Bifurcacion cofre mapa1 mapa2) = contieneTesoro (objetosDe cofre)
hayTesoroEn (d:ds) (Bifurcacion cofre mapa1 mapa2) = if esIzq d
                                                     then hayTesoroEn ds mapa1
                                                     else hayTesoroEn ds mapa2

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _ = False

caminoAlTesoro :: Mapa -> [Dir]
--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin cofre) = []
caminoAlTesoro (Bifurcacion cofre mapa1 mapa2) = if contieneTesoro (objetosDe cofre)
                                                 then []
                                                 else if hayTesoro (mapa1)
                                                    then Izq : caminoAlTesoro mapa1
                                                    else Der : caminoAlTesoro mapa2

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin cofre) = []
caminoDeLaRamaMasLarga (Bifurcacion cofre mapa1 mapa2) = if heightMapa mapa1 > heightMapa mapa2
                                                         then Izq : caminoDeLaRamaMasLarga mapa1
                                                         else Der : caminoDeLaRamaMasLarga mapa2

heightMapa :: Mapa -> Int
--Dado un mapa devuelve su altura.
heightMapa (Fin cofre)        = 0
heightMapa (Bifurcacion cofre mapa1 mapa2) = 1 + max (heightMapa mapa1) (heightMapa mapa2)

tesorosPorNivel :: Mapa -> [[Objeto]]
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin cofre) = [objetosDe cofre]
tesorosPorNivel (Bifurcacion cofre mapa1 mapa2) = objetosDe cofre : tesorosPorNivel mapa1 ++ tesorosPorNivel mapa2

todosLosCaminos :: Mapa -> [[Dir]]
--Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin cofre) = []
todosLosCaminos (Bifurcacion cofre mapa1 mapa2) = [[]] ++ consACada Izq (todosLosCaminos mapa1) ++ consACada Der (todosLosCaminos mapa2)

consACada :: a -> [[a]] -> [[a]]
consACada n []       = []
consACada n (x:xs)   = (n:x) : consACada n xs

-- Naves Espaciales

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

data Nave = N (Tree Sector)
    deriving Show

naveEjemplo = N sectorEjemplo 

sectorEjemplo :: Tree Sector 
sectorEjemplo =  NodeT sector1 
                        (NodeT sector2 
                                (NodeT sector4 
                                    EmptyT 
                                    EmptyT)
                                EmptyT)
                        (NodeT sector3
                            EmptyT
                            EmptyT)

sector1 = S "sector1" [LanzaTorpedos, (Motor 4)] ["Carlos"]
sector2 = S "sector2" [LanzaTorpedos, (Almacen ([Comida, Torpedo, Oxigeno]))] ["Julio"] 
sector3 = S "sector3" [(Almacen ([Comida, Oxigeno, Torpedo]))] ["Juan"]
sector4 = S "sector4" [(Motor 2)] ["Buchu", "Yoel", "Carlos"]

sectores :: Nave -> [SectorId]
-- Propósito: Devuelve todos los sectores de la nave.
sectores (N t) = sectoresT t

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT = []
sectoresT (NodeT x t1 t2) = idSector x : sectoresT t1 ++ sectoresT t2

idSector :: Sector -> SectorId
idSector (S id _ _) = id

poderDePropulsion :: Nave -> Int
--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion (N t) = poderDePropulsionT t

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT EmptyT = 0
poderDePropulsionT (NodeT x t1 t2) = poderDePropulsionS x + poderDePropulsionT t1 + poderDePropulsionT t2

poderDePropulsionS :: Sector -> Int
poderDePropulsionS (S _ componentes _) = poderDePropulsionC componentes

poderDePropulsionC :: [Componente] -> Int
poderDePropulsionC [] = 0
poderDePropulsionC (x:xs) = if esMotor x
                            then poderDeMotor x + poderDePropulsionC xs
                            else poderDePropulsionC xs

esMotor :: Componente -> Bool
esMotor (Motor x) = True
esMotor _ = False

poderDeMotor :: Componente -> Int
poderDeMotor (Motor x) = x
poderDeMotor _ = 0


barriles :: Nave -> [Barril]
--Propósito: Devuelve todos los barriles de la nave.
barriles (N t) = barrilesT t

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT s t1 t2) = barrilesS s ++ barrilesT t1 ++ barrilesT t2

barrilesS :: Sector -> [Barril]
barrilesS (S _ componentes _) = barrilesC componentes

barrilesC :: [Componente] -> [Barril]
barrilesC [] = []
barrilesC (x:xs) = if esAlmacen x
                   then barrilesDeAlmacen x ++ barrilesC xs
                   else barrilesC xs

esAlmacen :: Componente -> Bool
esAlmacen (Almacen x) = True
esAlmacen _ = False

barrilesDeAlmacen :: Componente -> [Barril]
barrilesDeAlmacen (Almacen x) = x
barrilesDeAlmacen _ = []

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector componentes sectorId (N t) = N (agregarASectorT t componentes sectorId)

agregarASectorT :: Tree Sector -> [Componente] -> SectorId -> Tree Sector
agregarASectorT EmptyT _ _ = EmptyT
agregarASectorT (NodeT x t1 t2) componentes id = NodeT (agregarASectorS x id componentes) (agregarASectorT t1 componentes id) (agregarASectorT t2 componentes id) 

agregarASectorS :: Sector -> SectorId -> [Componente] -> Sector
agregarASectorS    sector id componentes = if idSector sector == id      
                                                 then sectorConNuevosComponentes sector componentes  
                                                 else sector

sectorConNuevosComponentes :: Sector -> [Componente] -> Sector
sectorConNuevosComponentes (S id cs tps) componentes = S id componentes tps 

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA trip sectoresId (N t) = N (agregarASectoresT t sectoresId trip)

agregarASectoresT :: Tree Sector -> [SectorId] -> Tripulante -> Tree Sector
agregarASectoresT EmptyT _ _                      = EmptyT
agregarASectoresT (NodeT x t1 t2) sectoresId trip = NodeT (agregarTripulanteASectorS x sectoresId trip) (agregarASectoresT t1 sectoresId trip) (agregarASectoresT t2 sectoresId trip)  

agregarTripulanteASectorS :: Sector -> [SectorId] -> Tripulante -> Sector
agregarTripulanteASectorS sector sectoresId trip = if pertenece (idSector sector) sectoresId
                                                   then agregarTripulanteA trip sector
                                                   else sector 

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs


agregarTripulanteA :: Tripulante -> Sector -> Sector
agregarTripulanteA trip (S id cs tps) = S id cs (trip:tps)

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- Propósito: Devuelve los sectores en donde aparece un tripulante dado
sectoresAsignados trip (N t) = sectoresDe_ConTripulanteDado t trip

sectoresDe_ConTripulanteDado :: Tree Sector -> Tripulante -> [SectorId]
sectoresDe_ConTripulanteDado EmptyT _ = []
sectoresDe_ConTripulanteDado (NodeT x t1 t2) trip = if esTripulanteDe x trip
                                                    then idSector x : sectoresDe_ConTripulanteDado t1 trip ++ sectoresDe_ConTripulanteDado t2 trip
                                                    else sectoresDe_ConTripulanteDado t1 trip ++ sectoresDe_ConTripulanteDado t2 trip

esTripulanteDe :: Sector -> Tripulante -> Bool
esTripulanteDe (S _ _ tps) trip = pertenece trip tps

tripulantes :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes (N t) = sinRepetidos (tripulantesDeT t)

tripulantesDeT :: Tree Sector -> [Tripulante]
tripulantesDeT EmptyT = []
tripulantesDeT (NodeT x t1 t2) = tripulantesDeS x ++ tripulantesDeT t1 ++ tripulantesDeT t2

tripulantesDeS :: Sector -> [Tripulante]
tripulantesDeS (S _ _ tps) = tps

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs

type Presa = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre = String -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
    deriving Show

data Manada = M Lobo 
    deriving Show

--Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
--crías. 

manada1 = M (Cazador "Alfa" ["Ciervo", "Liebre", "Conejo" ]
        (Explorador "Beta" ["Bosque", "Lago", "Pradera"] (Cria "Cachorro1") (Cria "Cachorro2"))
        (Explorador "Gamma" ["Montaña", "Río", "Desierto"] (Cria "Cachorro3") (Cria "Cachorro4"))
        (Cria "Cachorro5"))

manada2 = M (Cazador "Alfa" ["Ciervo", "Liebre", "Conejo" ]
        (Explorador "Beta" ["Bosque", "Lago", "Pradera"] (Cria "Cachorro1") (Cria "Cachorro2"))
        (Explorador "Gamma" ["Bosque", "Río", "Desierto"] (Cria "Cachorro3") (Cazador "Alfon" ["Ciervo", "Liebre", "Conejo", "Ciervo", "Liebre", "Conejo" ] (Cria "Cachorro5") (Cria "Cachorro6") (Cria "Cachorro7")))
        (Cria "Cachorro4"))

buenaCaza :: Manada -> Bool
--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza m = cantidadDeAlimentoM m > cantidadDeCriasM m

cantidadDeAlimentoM :: Manada -> Int
cantidadDeAlimentoM (M lobo) = cantidadDeAlimentoL lobo

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cria _) = 0
cantidadDeAlimentoL (Explorador _ _ l1 l2) = cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2
cantidadDeAlimentoL (Cazador _ presas l1 l2 l3) = cantidadAlimento presas + cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2 + cantidadDeAlimentoL l3

cantidadAlimento :: [Presa] -> Int
cantidadAlimento p = length p

cantidadDeCriasM :: Manada -> Int
cantidadDeCriasM (M lobo) = cantidadDeCriasL lobo

cantidadDeCriasL :: Lobo -> Int
cantidadDeCriasL (Cria _) = 1
cantidadDeCriasL (Explorador _ _ l1 l2) = cantidadDeCriasL l1 + cantidadDeCriasL l2
cantidadDeCriasL (Cazador _ _ l1 l2 l3) = cantidadDeCriasL l1 + cantidadDeCriasL l2 + cantidadDeCriasL l3


