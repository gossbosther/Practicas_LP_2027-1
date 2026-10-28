module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola x y = potenciaColaMio x y 1 where  
  potenciaColaMio base 0 acc = acc
  potenciaColaMio base exponente acc = potenciaColaMio base (exponente - 1) (acc * base)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 f = f 1
potenciaCPS base exponente f = potenciaCPS base (exponente - 1) (\v -> f(base * v))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = longitudColaMio xs 0 where
  longitudColaMio [] acc = acc
  longitudColaMio (_ : xs) acc = longitudColaMio xs (acc + 1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] f = f 0
longitudCPS (x:xs) f = longitudCPS xs (\v -> f(1 + v))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola xs = reversaColaMio xs [] where
  reversaColaMio [] xs = xs
  reversaColaMio (x:xs) ys = reversaColaMio xs (x : ys)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] f = f []
reversaCPS (x:xs) f = reversaCPS xs (\v -> f(v ++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xs = concatenaColaMio xs [] where
  concatenaColaMio [] acc = acc
  concatenaColaMio (x:xs) ys = concatenaColaMio xs (ys ++ x)

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] f = f []
concatenaCPS (x:xs) f = concatenaCPS xs (\v -> f(x ++ v))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = intercalaColaMio xs ys [] where
  intercalaColaMio [] ys acc = acc ++ ys
  intercalaColaMio xs [] acc = acc ++ xs
  intercalaColaMio (x:xs) (y:ys) acc = intercalaColaMio xs ys (acc ++ [x] ++ [y])

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys f = f ys
intercalaCPS xs [] f = f xs
intercalaCPS (x:xs) (y:ys) f = intercalaCPS xs ys (\v -> f(x : y : v))
