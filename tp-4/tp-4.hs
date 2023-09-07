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