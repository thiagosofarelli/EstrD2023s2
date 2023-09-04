data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

pizza1 = Capa (Aceitunas 10) (Capa Queso Prepizza)

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

