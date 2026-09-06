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

freshName :: [String] -> String

sust :: ASA -> String -> ASA -> ASA

sustMany :: ASA -> [Binding] -> ASA

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
