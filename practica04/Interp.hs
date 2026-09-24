module Interp where

import Grammars
import Data.Maybe (Maybe(Nothing))

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] x = Nothing
curryFun [x] y = Just(Fun x y)
curryFun (x:xs) y 
  | x `elem` xs = Nothing
  | otherwise = case curryFun xs y of 
      Just v ->  Just(Fun x v)
      Nothing -> Nothing

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp x [] = Nothing
curryApp e xs = Just(foldl App e xs)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [x] = Nothing
binaryOp op (x:xs) = Just(foldl op x xs)


-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS i) = Just(Id i)
desugar (NumS n) = Just(Num n)
desugar (BooleanS b) = Just(Boolean b)
desugar (AddS xs) 
  | Just xs' <- traverse desugar xs = binaryOp Add xs'
  | otherwise = Nothing
desugar (SubS xs) 
  | Just xs' <- traverse desugar xs = binaryOp Sub xs'
  | otherwise = Nothing
desugar (NotS x)
  | Just x' <- desugar x = Just(Not x')
  | otherwise = Nothing
desugar (LetS x y z)
  | Just y' <- desugar y 
  , Just z' <- desugar z = Just (App(Fun x z') y')
  | otherwise = Nothing
desugar (LetStarS ((x, y):xs) z) = desugar (LetS x y (LetStarS xs z))
desugar (LetStarS [] z) = desugar z
desugar (FunS v b) = do
  b' <- desugar b
  curryFun v b'
desugar (AppS e1 e2) = do
  e1' <- desugar e1
  e2' <- traverse desugar e2
  curryApp e1' e2'


-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((y,z):yz)
  | x == y = Just z
  | otherwise = lookupEnv x yz

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep xs (Id i) = lookupEnv i xs
bigStep _ (Num n) = Just(NumV n)
bigStep _ (Boolean b) = Just(BooleanV b)
bigStep x (Add e1 e2) = do
  e1' <- bigStep x e1
  e2' <- bigStep x e2
  case (e1', e2') of 
    (NumV n, NumV m) -> Just(NumV (n + m))
    _ -> Nothing
bigStep x (Sub e1 e2) = do
  e1' <- bigStep x e1 
  e2' <- bigStep x e2 
  case (e1', e2') of
    (NumV n, NumV m) -> Just(NumV (max 0 (n - m)))
    (_, _) -> Nothing
bigStep x (Not e) = do
  e' <- bigStep x e
  case e' of 
    BooleanV b -> Just(BooleanV (not b))
    NumV _ -> Just(BooleanV False)
    _ -> Nothing
bigStep env (Fun x e) = Just(ClosureV x e env)
bigStep env (App e1 e2) = do
  e1' <- bigStep env e1
  e2' <- bigStep env e2
  case e1' of
    ClosureV n a env' ->
      let env'' = (n , e2') : env'
      in bigStep env'' a
    _ -> Nothing
