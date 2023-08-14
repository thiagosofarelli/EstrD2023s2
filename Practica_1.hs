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
