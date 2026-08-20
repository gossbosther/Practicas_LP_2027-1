module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double

sumaCuadradosPares :: [Int] -> Int

aplicaTresVeces :: (a -> a) -> a -> a

varianza2 :: Double -> Double -> Double

clasificaTemperatura :: Int -> String

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
