-- Marin Parra Jose Guadalupe de Jesus
module Practica1 where

import Data.List (genericLength)

areaLateral :: Float -> Float -> Float -> Float
areaLateral a b c = 2*(a+b)*c

areaTotal :: Float -> Float -> Float
areaTotal r g = a + b
  where
    a = pi*r*g
    b = pi*r^2

distancia :: (Float , Float ) -> (Float , Float ) -> Float
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)

impl :: Bool -> Bool -> Bool
impl True False = False
impl False True = True
impl True True = True
impl False False = True

calculadora :: String -> (Int ,Int) -> Int
calculadora "first" (a,b) = a
calculadora "last" (a,b) = b
calculadora "sum" (a,b) = a+b
calculadora "dif" (a,b) = a-b
calculadora "mul" (a,b) = a*b
calculadora "div" (a,b) = div a b
calculadora "pow" (a,b) = a^b

loki :: Int -> Bool -> String
loki x False
  | x >= 15 && x <= 25 = "Sale a jugar"
  | otherwise = "No sale a jugar"
loki x True
  | x >= 20 && x <= 30 = "Sale a jugar"
  | otherwise = "No sale a jugar"

numeroFormas :: Int -> Int
numeroFormas 0 = 1
numeroFormas 1 = 1
numeroFormas 2 = 2
numeroFormas n = numeroFormas(n-1) + numeroFormas(n-2) + numeroFormas(n-3)

divisoresPropios :: Integer -> [Integer]
divisoresPropios x = [y | y <- [1..x-1], x `mod` y == 0]

cribaEratostenes ::  Int -> [Int]
cribaEratostenes n = cribaAux [x | x <- [2..n]] 0
--Funcion auxiliar para cribaEratostenes
cribaAux :: [Int] -> Int -> [Int]
cribaAux list a
  | a == length list-1 = list
  | otherwise = cribaAux[x | x <- list,(x `mod` list!!a) /= 0 || x == list!!a] (a+1)

perfectos :: [Int] -> [Int]
perfectos [] = []
perfectos [1,2,3,4,5] = [0]
perfectos (x:xs) =
  if perf x /= x
    then perfectos xs
    else x : perfectos xs
  where
    perf 0 = 0
    perf 1 = 0
    perf x = foldl1 (+) [n | n <- [1..x-1],x `mod` n == 0]

aproxima :: Float -> Float
aproxima 0 = 1
aproxima n = (1/(factorial n)) + aproxima(n-1)
  where
    factorial 1 = 1
    factorial n = n * factorial (n - 1)

data Figura = Triangulo Float Float Float | Cuadrado Float | Rectangulo Float Float | Rombo Float Float Float
  | Paralelogramo Float Float Float | Circulo Float | Elipse Float Float

area :: Figura -> Float
area (Triangulo a b c) = sqrt(s*(s-a)*(s-b)*(s-c))
  where
    s = (a+b+c)/2
area (Cuadrado a) = a * a
area (Rectangulo a b) = a * b
area (Rombo a dm d) = (dm*d) / 2
area (Paralelogramo a b h) = b * h
area (Circulo r) = pi * r^2
area (Elipse a b) = pi * a * b
