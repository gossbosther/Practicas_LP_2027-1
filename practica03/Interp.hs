module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]

-- Para eliminar duplicados de las listas (equivalente a nub de Data.List)
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

-- Aqui obtenemos las variables libres de una expresion, hay que ir respetando los alcances de Let y Let*
freeVars (Id x) = [x]
freeVars (Num _) = []
freeVars (Boolean _) = []
freeVars (And args) = unique (concatMap freeVars args)
freeVars (Or args) = unique (concatMap freeVars args)
freeVars (Add args) = unique (concatMap freeVars args)
freeVars (Sub args) = unique (concatMap freeVars args)
freeVars (Mul args) = unique (concatMap freeVars args)
freeVars (Div args) = unique (concatMap freeVars args)
freeVars (Lt args) = unique (concatMap freeVars args)
freeVars (Gt args) = unique (concatMap freeVars args)
freeVars (Le args) = unique (concatMap freeVars args)
freeVars (Ge args) = unique (concatMap freeVars args)
freeVars (Expt a b) = unique (freeVars a ++ freeVars b)
freeVars (EqP a b) = unique (freeVars a ++ freeVars b)
freeVars (Not a) = freeVars a
freeVars (Add1 a) = freeVars a
freeVars (Sub1 a) = freeVars a
freeVars (ZeroP a) = freeVars a
freeVars (Let binds body) =
  let (vars, exprs) = unzip binds
      fvExprs = concatMap freeVars exprs
      fvBody = filter (`notElem` vars) (freeVars body)
  in unique (fvExprs ++ fvBody)
freeVars (LetStar [] body) = freeVars body
freeVars (LetStar ((v, e):rest) body) =
  let fvE = freeVars e
      fvRest = filter (/= v) (freeVars (LetStar rest body))
  in unique (fvE ++ fvRest)

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
names (Let x y) = []
names (LetStar x y) = []

names2 :: [ASA] -> [String]
names2 [] = []
names2 (x:xs) = names x ++ names2 xs

freshName :: [String] -> String

sust :: ASA -> String -> ASA -> ASA
sust (Num n) _ _ = Num n
sust (Boolean b) _ _ = Boolean b
sust (Id x) y z 
    | x == y = z
    | otherwise = Id x
sust (Add n) x s = Add sust2 n x s
sust (Sub n) x s = Sub sust2 n x s
sust (Mul n) x s = Mul sust2 n x s
sust (Div n) x s = Div sust2 n x s
sust (Lt n) x s = Lt sust2 n x s
sust (Gt n) x s = Gt sust2 n x s
sust (Le n) x s = Le sust2 n x s
sust (Expt e1 e2) x s = Expt (sust e1 x s) (sust e2 x s)
sust (EqP e1 e2) x s = EqP (sust e1 x s) (sust e2 x s)
sust (Not e) x s = Not (sust e x s)
sust (Add1 e) x s = Add1 (sust e x s)
sust (Sub1 e) x s = Sub1 (sust e x s)
sust (Let x y) z w 
    | x == z = Let (sustBinding x z w) 
    | x /= z && notElem z (freeVars w) = Let (sustBinding x z w) (sust y x w)
    | x /= z && elem z (freeVars w) = 
        let t = freshName y in Let t (sustBinding x z w) sust (sust y x (Id t) ) z w

sust2 :: [ASA] -> String -> ASA -> [ASA]
sust2 [] _ _ = []
sust2 (x:xs) y z = sust x y z : sust2 xs y z

sustMany :: ASA -> [Binding] -> ASA


-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
