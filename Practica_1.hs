-- Práctica de ejercicios # 1 - Tipos algebraicos

-- Ejercicio 2

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
