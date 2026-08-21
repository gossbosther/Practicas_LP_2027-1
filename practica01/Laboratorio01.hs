module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double

sumaCuadradosPares :: [Int] -> Int

-- 3 Aplicar una función 3 veces al mismo valor

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces :: f x = f (f (f x))


-- 4 Calcula la varianza de 2 datos usando una media local

varianza2 :: Double -> Double -> Double
varianza2 x y = ((x - m)^2 + (y - m)^2)/2
  where m = (x + y)/2

-- 5 Clasificar temperatura con guardias e ifs
clasificaTemperatura :: Int -> String
clasificaTemperatura t 
  | t <= 0      = "frio extremo"
  | t <= 15     = "frio"
  | t <= 25     = "templado"
  | t <= 35     = "calido"
  | otherwise   = "calor extremo"


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
