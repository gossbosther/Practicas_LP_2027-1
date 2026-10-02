module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] x = Nothing
curryFun [x] y = Just(Fun x y)
curryFun (x:xs) y
  | x `elem` xs = Nothing
  | otherwise = 
    let 
      Just fxs = curryFun xs y
    in Just(Fun x fxs)

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp x [] = Nothing
curryApp e xs = Just(foldl App e x)

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [x] = Nothing
binaryOp op (x:xs) = Just(foldl op x xs)

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond (CondS [] e) = desugar e
desugarCond (CondS (c, r) e) = desugar(IfS c r e)
desugarCond (CondS ((c, r):xs) e) = desugar(IfS c r (desugarCond xs e))

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (Ids i) = Just(Id i)
desugar (NumS n) = Just(Num n)
desugar (BooleanS b) = Just(Boolean b)
desugar (AddS xs) 
  | Just xs' <- traverse desugar xs = binaryOp Add xs'
  | otherwise = Nothing
desugar (SubS xs)
  | Just xs' <- traverse desugar xs = binaryOp Sub xs'
  | otherwise = Nothing
desugar (NotS x)
  | Just x <- desugar x = just(Not x')
  | otherwise = Nothing
desugar (LetS x y z)
  | (Just y', Just z') <- (desugar y, desugar z) = Just(App(Fun x z') y')
  | otherwise = Nothing
desugar (LetStarS ((x,y):xs) z) = desugar (LetS x y (LetStarS xs z))
desugar (LetStarS [] z) = desugar z
desugar (FunS v b) 
  | Just b' <- desugar b = curryFun v b'
  | otherwhise = Nothing
desugar (AppS e1 e2)
  | (Just e1', Just e2') <- (desugar e1, traverse desugar e2) = curryApp e1' e2'
  | otherwise = Nothing
desugar (CondS x e) = desugarCond (CondS x e)
desugar (IfS c t e) =
  let 
    Just vc = desugar c
    Just vt = desugar t
    Just ve = desugar e
  in Just(If vc vt ve)
-- Falta el desugar de letRec

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookenv x ((y, z):yz)
  | x == y = Just z
  | otherwise = lookupEnv x yz

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (NumV n) = Just(NumV n)
strict (BooleanV b) = Just(BooleanV b)
strict (ClosureV n a e) = Just(Cosure n a e)
strict (ExprV x e) 
  | Just e' <- bigStep e x = strict e
  | otherwise = Nothing

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep e (Id s) = lookupEnv s e
bigStep _ (Num n) = Just(Num n)
bigStep _ (Boolean b) = Just(Boolean b)
bigStep e (Add e1 e2) 
  | (Just e1', Just e2') <- (bigStep e e1, bigStep e e1) = 
    let 
      Just (NumV n) = strict e1'
      Just (NumV m) = strict e2'
    in Just(NumV (n + m))
  | otherwise = Nothing
bigStep e (Sub e1 e1)
  | (Just e1', Just e2') <- (bigStep e e1, bigStep e e2) = 
    let 
      Just (NumV n) = strict e1'
      Just (NumV m) = strict e2'
    in Just(NumV max(0, n - m))
  | otherwise = Nothing
bigStep e (Not b) 
  | Just b' <- bigStep e b = 
    let 
      Just(BooleanV b'') = strict b
    in Just(BooleanV not b'')
  | otherwise = Nothing
bigStep env (Fun x e ) = Just(ClosureV x e env)
bigStep env (App e1 e2) 
  | Just e1' <- bigStep env e1 = 
    let 
      Just(Closure n a env') = strict e1'
      env'' = (n, (ExprV e2, env)): env'
    in bigStep env'' a
  | otherwise = Nothing
bigStep env (If a t e)
  | Just a' <- bigstep env a = 
    let 
      Just(Boolean b) = strict a'
    in if b 
      then bigStep env t 
      else bigStep env e
  | otherwise = Nothing