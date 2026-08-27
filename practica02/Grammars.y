{
module Grammars where

import Lexer (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      nat             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMul }
      '/'             { TokenDiv }
      "and"           { TokenAnd }
      "or"            { TokenOr }
      "not"           { TokenNot }
      "add1"          { TokenAdd1 }
      "sub1"          { TokenSub1 }
      "zero?"         { TokenZeroP }
      "expt"          { TokenExpt }
      '<'             { TokenLT }
      '>'             { TokenGT }
      "<="            { TokenLE }
      ">="            { TokenGE }
      "eq"            { TokenEq }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

ASA : nat                      { Num $1 }
    | bool                     { Boolean $1 }

-- RETO 2:
-- Agrega las producciones para:
--   * operadores n-arios con al menos dos argumentos;
--   * operadores estrictamente binarios: expt y eq;
--   * operadores unarios: not, add1, sub1, zero?.

    | '(' '+' n ')'           { Add $3 }
    | '(' '-' n ')'           { Sub $3 }
    | '(' '*' n ')'           { Mul $3 }
    | '(' '/' n ')'           { Div $3 }
    | '(' "and" n ')'         { And $3 }
    | '(' "or" n ')'          { Or $3 }
    | '(' "not" ASA ')'       { Not $3 }
    | '(' "add1" ASA ')'      { Add1 $3 }
    | '(' "sub1" ASA ')'      { Sub1 $3 }
    | '(' "zero?" ASA ')'     { ZeroP $3 }
    | '(' "expt" ASA ASA ')'  { Expt $3 $4 }
    | '(' '<' n ')'           { Lt $3 }
    | '(' '>' n ')'           { Gt $3 }
    | '(' "<=" n ')'          { Le $3 }
    | '(' ">=" n ')'          { Ge $3 }
    | '(' "eq" ASA ASA ')'    { EqP $3 $4 }

-- RETO 3:
-- Agrega un no terminal para representar dos o mas argumentos.
-- El resultado debe ser una lista de ASA.

{
parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

data ASA
  = Num Int
  | Boolean Bool
  | And [ASA]
  | Or [ASA]
  | Add [ASA]
  | Sub [ASA]
  | Mul [ASA]
  | Div [ASA]
  | Lt [ASA]
  | Gt [ASA]
  | Le [ASA]
  | Ge [ASA]
  | Expt ASA ASA
  | EqP ASA ASA
  | Not ASA
  | Add1 ASA
  | Sub1 ASA
  | ZeroP ASA
  deriving (Eq, Show)
}
