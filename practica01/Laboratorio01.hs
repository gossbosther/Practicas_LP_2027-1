module Laboratorio01 where

-- 1 Calcula la distancia euclidiana desde el punto (x, y) al origen.

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt(x^2 + y^2)

-- 2 Calcula la suma de los cuadrados de aquellos elementos que sean pares.

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares [] = 0
sumaCuadradosPares (x:xs)
  | even x = (x^2) + sumaCuadradosPares xs
  | otherwise = sumaCuadradosPares xs

-- 3 Aplicar una función 3 veces al mismo valor

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))


-- 4 Calcula la varianza de 2 datos usando una media local

varianza2 :: Double -> Double -> Double
varianza2 x y = ((x - m)^2 + (y - m)^2)/2
  where m = (x + y)/2

-- 5 Clasificar temperatura con guardias e ifs
clasificaTemperatura :: Int -> String
clasificaTemperatura t 
  | t < 0  = "frio extremio"
  | t <= 10 = "frio"
  | otherwise = if t<= 20
                then "templado"
                else if t<= 30
                  then "calido"
                  else "calor extremo"


intercala :: a -> [a] -> [a]
intercala _ [] = []
intercala _ [x] = [x]
intercala sep (x:xs) = x : sep : intercala sep xs

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma e1 e2) = evalua e1 + evalua e2
evalua (Producto e1 e2) = evalua e1 * evalua e2
