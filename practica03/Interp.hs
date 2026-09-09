module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]
freeVars (Id x) = [x]
freeVars (Num _) = []
freeVars (Boolean _) = []
freeVars (And e) = free e
freeVars (Or e) = free e
freeVars (Add e) = free e
freeVars (Sub e) = free e
freeVars (Mul e) = free e
freeVars (Div e) = free e
freeVars (Lt e) = free e
freeVars (Gt e) = free e
freeVars (Le e) = free e
freeVars (Ge e) = free e
freeVars (Expt e1 e2) = freeVars e1 ++ freeVars e2
freeVars (EqP e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Not e) = freeVars e
freeVars (Add1 e) = freeVars e
freeVars (Sub1 e) = freeVars e
freeVars (ZeroP e) = freeVars e
freeVars (Let x y) = freeBindingLet x ++ filtrar (nombresVars x) (freeVars y)
freeVars (LetStar x y) = freeBinding x y

free :: [ASA] -> [String]
free [] = []
free (x:xs) = freeVars x ++ free xs

freeBindingLet ::[Binding] -> [String]
freeBindingLet [] = []
freeBindingLet ((_, y):xs) = freeVars y ++ freeBindingLet xs

nombresVars :: [Binding] -> [String]
nombresVars [] = []
nombresVars ((x, _):xs) = x : nombresVars xs

filtrar :: [String] -> [String] -> [String]
filtrar [] x = x
filtrar (x:xs) y = filtrar xs (filter (/= x) y)

freeBinding :: [Binding] -> ASA -> [String]
freeBinding [] x = freeVars x
freeBinding ((x, y):xs) z = freeVars y ++ filter (/= x) (freeBinding xs z)


names :: ASA -> [String]
names (Num _) = []
names (Boolean _) = []
names (Id x) = [x]
names (And n) = names2 n
names (Or n) = names2 n
names (Add n) = names2 n
names (Sub n) = names2 n
names (Mul n) = names2 n
names (Div n) = names2 n
names (Lt n) = names2 n
names (Gt n) = names2 n
names (Le n) = names2 n
names (Ge n) = names2 n
names (Expt e1 e2) = names e1 ++ names e2
names (EqP e1 e2) = names e1 ++ names e2
names (Not e) = names e
names (Add1 e) = names e
names (Sub1 e) = names e
names (ZeroP e) = names e
names (Let x y) = namesBinding x ++ names y
names (LetStar x y) = namesBinding x ++ names y

names2 :: [ASA] -> [String]
names2 [] = []
names2 (x:xs) = names x ++ names2 xs

namesBinding :: [Binding] -> [String]
namesBinding [] = []
namesBinding ((x, y):xs) = x : names y ++ namesBinding xs

freshName :: [String] -> String
freshName used = choose 0
  where
    choose n
      | candidate `elem` used = choose (n + 1)
      | otherwise = candidate
      where
        candidate = "_x" ++ show (n :: Int)

sust :: ASA -> String -> ASA -> ASA
sust (Num n) _ _ = Num n
sust (Boolean b) _ _ = Boolean b
sust (Id x) y z 
    | x == y = z
    | otherwise = Id x
sust (And n) x s = And (sust2 n x s)
sust (Add n) x s = Add (sust2 n x s)
sust (Sub n) x s = Sub (sust2 n x s)
sust (Mul n) x s = Mul (sust2 n x s)
sust (Div n) x s = Div (sust2 n x s)
sust (Lt n) x s = Lt (sust2 n x s)
sust (Gt n) x s = Gt (sust2 n x s)
sust (Le n) x s = Le (sust2 n x s)
sust (Expt e1 e2) x s = Expt (sust e1 x s) (sust e2 x s)
sust (EqP e1 e2) x s = EqP (sust e1 x s) (sust e2 x s)
sust (Not e) x s = Not (sust e x s)
sust (Add1 e) x s = Add1 (sust e x s)
sust (Sub1 e) x s = Sub1 (sust e x s)
sust (ZeroP e) x s = ZeroP (sust e x s)
sust (Let x y) z w 
    | z `elem` nombresVars x = Let (sustBindings x z w) y
    | null (filter (`elem` freeVars w) (nombresVars x)) = Let (sustBindings x z w) (sust y z w)
    | otherwise = 
        let conflicto = head (filter (`elem` freeVars w) (nombresVars x))
            t = freshName (names (Let x y) ++ freeVars w ++ [z])
            renombraBinding (v, e) = (if v == conflicto then t else v, sust e conflicto (Id t))
            x' = map renombraBinding x
            y' = sust y conflicto (Id t)
        in Let (sustBindings x' z w) (sust y' z w)
sust (LetStar [] body) z w = LetStar [] (sust body z w)
sust (LetStar ((v, e):bs) body) z w
  | v == z = LetStar ((v, sust e z w) : bs) body
  | v `elem` freeVars w =
      let t     = freshName (names (LetStar ((v, e):bs) body) ++ freeVars w ++ [z])
          e'    = sust e z w
          bs'   = map (\(var, expr) -> (if var == v then t else var, sust expr v (Id t))) bs
          body' = sust body v (Id t)
      in case sust (LetStar bs' body') z w of
           LetStar bs'' body'' -> LetStar ((t, e') : bs'') body''
           _                   -> LetStar ((t, e') : bs') body
    | otherwise =
        let e' = sust e z w
        in case sust (LetStar bs body) z w of
            LetStar bs' body' -> LetStar ((v, e') : bs') body'
            _                 -> LetStar ((v, e') : bs) body
        

sust2 :: [ASA] -> String -> ASA -> [ASA]
sust2 [] _ _ = []
sust2 (x:xs) y z = sust x y z : sust2 xs y z

sustBindings :: [Binding] -> String -> ASA -> [Binding]
sustBindings [] _ _ = []
sustBindings ((x, y):xs) z w = (x, sust y z w) : sustBindings xs z w

renombrarStar :: [Binding] -> String -> String -> [Binding]
renombrarStar [] _ _ = []
renombrarStar ((x, y):xs) z w 
    | x == z = (w, sust y z (Id w)) : sustBindings xs z (Id w)
    | otherwise = (x, sust y z (Id w)) : renombrarStar xs z w

sustMany :: ASA -> [Binding] -> ASA
sustMany (Num n) _ = Num n
sustMany (Boolean b) _ = Boolean b
sustMany (Id x) y = buscarSust x y
sustMany (And n) x = And (sustMany2 n x)
sustMany (Add n) x = Add (sustMany2 n x)
sustMany (Sub n) x = Sub (sustMany2 n x)
sustMany (Mul n) x = Mul (sustMany2 n x)
sustMany (Div n) x = Div (sustMany2 n x)
sustMany (Lt n) x = Lt (sustMany2 n x)
sustMany (Gt n) x = Gt (sustMany2 n x)
sustMany (Le n) x = Le (sustMany2 n x)
sustMany (Ge n) x = Ge (sustMany2 n x)
sustMany (Expt e1 e2) x = Expt (sustMany e1 x) (sustMany e2 x)
sustMany (EqP e1 e2) x = EqP (sustMany e1 x) (sustMany e2 x)
sustMany (Not e) x = Not (sustMany e x)
sustMany (Add1 e) x = Add1 (sustMany e x)
sustMany (Sub1 e) x = Sub1 (sustMany e x)
sustMany (ZeroP e) x = ZeroP (sustMany e x)
sustMany (Let x y) z = 
    let x' = sustManyBindings x z
        z' = filter (\(v, _) -> v `notElem` nombresVars x) z
    in Let x' (sustMany y z')
sustMany (LetStar x y) z = 
    let x' = sustManyBindings x z
        z' = filter (\(v, _) -> v `notElem` nombresVars x) z
    in LetStar x' (sustMany y z')

buscarSust :: String -> [Binding] -> ASA
buscarSust x [] = Id x
buscarSust x ((y, z):ys) 
    | x == y = z
    | otherwise = buscarSust x ys

sustMany2 :: [ASA] -> [Binding] -> [ASA]
sustMany2 [] _ = []
sustMany2 (x:xs) y = sustMany x y : sustMany2 xs y

sustManyBindings :: [Binding] -> [Binding] -> [Binding]
sustManyBindings [] _ = []
sustManyBindings ((x, y):xs) z = (x, sustMany y z) : sustManyBindings xs z



-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
bigStep (Num n) = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)
bigStep (Id _) = Nothing
bigStep (Add []) = Nothing
bigStep (Add args) = do
  vs <- mapM bigStep args
  ns <- mapM getNum vs
  Just (Num (sum ns))
bigStep (Sub []) = Nothing
bigStep (Sub [e]) = do
  v <- bigStep e
  _ <- getNum v
  Just (Num 0)
bigStep (Sub (x:xs)) = do
  v  <- bigStep x
  vs <- mapM bigStep xs
  n  <- getNum v
  ns <- mapM getNum vs
  Just (Num (max 0 (foldl (-) n ns)))
bigStep (Mul args) = do
  vs <- mapM bigStep args
  ns <- mapM getNum vs
  Just (Num (product ns))
bigStep (Div [])  = Nothing
bigStep (Div [_]) = Nothing -- Requiere al menos 2 operandos
bigStep (Div (x:xs)) = do
  v  <- bigStep x
  vs <- mapM bigStep xs
  n  <- getNum v
  ns <- mapM getNum vs
  if 0 `elem` ns
    then Nothing
    else Just (Num (foldl div n ns))
bigStep (Add1 e) = do
    v <- bigStep e
    n <- getNum v
    return (Num (n + 1))
bigStep (Sub1 e) = do
  v <- bigStep e
  n <- getNum v
  Just (Num (max 0 (n - 1)))
bigStep (Expt e1 e2) = do
  v1 <- bigStep e1
  v2 <- bigStep e2
  n1 <- getNum v1
  n2 <- getNum v2
  if n2 < 0 
    then Nothing 
    else Just (Num (n1 ^ n2))
bigStep (And xs) = do
    evs   <- bigStep2 xs
    bools <- mapM getBool evs
    return (Boolean (and bools))
bigStep (Or xs) = do
    evs   <- bigStep2 xs
    bools <- mapM getBool evs
    return (Boolean (or bools))
bigStep (Not e) = do
  v <- bigStep e
  case v of
    Boolean False -> Just (Boolean True)
    _ -> Just (Boolean False)
bigStep (ZeroP e) = do
    v <- bigStep e
    n <- getNum v
    return (Boolean (n == 0))
bigStep (EqP e1 e2) = do
  v1 <- bigStep e1
  v2 <- bigStep e2
  case (v1, v2) of
    (Num n1, Num n2) -> Just (Boolean (n1 == n2))
    (Boolean b1, Boolean b2) -> Just (Boolean (b1 == b2))
    _ -> Nothing  
bigStep (Lt xs) = do
  evs <- bigStep2 xs
  ns  <- mapM getNum evs
  if length ns < 2 then Nothing else Just (Boolean (ordenadoP (<) ns))
bigStep (Gt xs) = do
  evs <- bigStep2 xs
  ns  <- mapM getNum evs
  if length ns < 2 then Nothing else Just (Boolean (ordenadoP (>) ns))
bigStep (Le xs) = do
  evs <- bigStep2 xs
  ns  <- mapM getNum evs
  if length ns < 2 then Nothing else Just (Boolean (ordenadoP (<=) ns))
bigStep (Ge xs) = do
  evs <- bigStep2 xs
  ns  <- mapM getNum evs
  if length ns < 2 then Nothing else Just (Boolean (ordenadoP (>=) ns))
bigStep (Let [] body) = bigStep body
bigStep (Let bs body)
  | duplicadosP (map fst bs) = Nothing
  | otherwise = do
      vals <- mapM (\(_, e) -> bigStep e) bs
      let vars  = map fst bs
          subst = zip vars vals
          body' = foldl (\acc (v, val) -> sust acc v val) body subst
      bigStep body'

-- LetStar
bigStep (LetStar [] body) = bigStep body
bigStep (LetStar ((var, val):bs) body) = do
  v <- bigStep val
  case sust (LetStar bs body) var v of
    LetStar bs' body' -> bigStep (LetStar bs' body')
    expr              -> bigStep expr

getNum :: ASA -> Maybe Int
getNum (Num n) = Just n
getNum _ = Nothing

getBool :: ASA -> Maybe Bool
getBool (Boolean b) = Just b
getBool _ = Nothing

bigStep2 :: [ASA] -> Maybe [ASA]
bigStep2 [] = Just []
bigStep2 (x:xs) = do
    v  <- bigStep x
    vs <- bigStep2 xs
    return (v:vs)

getNumsList :: [ASA] -> Maybe [Int]
getNumsList []     = Just []
getNumsList (x:xs) = do
  n  <- getNum x
  ns <- getNumsList xs
  return (n:ns)

duplicadosP :: Eq a => [a] -> Bool
duplicadosP []     = False
duplicadosP (x:xs) = x `elem` xs || duplicadosP xs

ordenadoP :: (a -> a -> Bool) -> [a] -> Bool
ordenadoP _ []         = True
ordenadoP _ [_]        = True
ordenadoP op (x:y:rest) = x `op` y && ordenadoP op (y:rest)
