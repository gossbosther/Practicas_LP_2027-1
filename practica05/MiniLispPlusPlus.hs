module MiniLispPlusPlus where

import Control.Monad.IO.Class (liftIO)
import Grammars
import Interp
import Lexer
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)

-- RETO 5: integrar el combinador Y ----------------------------------------

-- Representa en el ASA del nucleo el combinador clasico:
--
-- Y = lambda f.
--       (lambda x. f (x x))
--       (lambda x. f (x x))
combinadorY :: ASA
combinadorY = 
  Fun "f" 
    (App 
      (Fun "x" (App (Id "f") (App (Id "x") (Id "x")))) 
      (Fun "x" (App (Id "f") (App (Id "x") (Id "x")))))

-- Evalua combinadorY en el ambiente vacio y asocia su valor con el nombre Y.
prelude :: Env
prelude = obtenerPrelude (bigStep [] combinadorY)

-- Funcion auxiliar para extraer el valor evaluado y evitar case of
obtenerPrelude :: Maybe Value -> Env
obtenerPrelude (Just v) = [("Y", v)]
obtenerPrelude Nothing = []

-- Integra el analisis, el desazucarado y la evaluacion desde prelude.
-- El resultado final debe pasar por strict antes de devolverse.
evalua :: String -> Maybe Value
evalua entrada
  | let astFuente = parse (lexer entrada),
    Just astNucleo <- desugar astFuente,
    Just val <- bigStep prelude astNucleo = strict val
  | otherwise = Nothing

-- Infraestructura provista. No forma parte de los retos.
repl :: IO ()
repl = runInputT defaultSettings loop

loop :: InputT IO ()
loop =
  getInputLine "MiniLisp++> " >>= maybe (pure ()) procesaEntrada

procesaEntrada :: String -> InputT IO ()
procesaEntrada ":q" = pure ()
procesaEntrada entrada =
  liftIO (maybe muestraBloqueo print (evalua entrada)) >> loop

muestraBloqueo :: IO ()
muestraBloqueo =
  putStrLn "Error: evaluacion bloqueada"

main :: IO ()
main = repl
