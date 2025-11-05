module Test where

import SASA
import Desugar
import Interp
import qualified Grammars
import qualified Lexer

-- Lista de ejemplos prehechos
examples :: [String]
examples =
  [ "(+ 2 3)"
  , "((lambda (x) (lambda (y) (+ x y))) 2 3)"
  , "(let ((x 2) (y 3)) (+ x y))"
  , "(let* ((x 2) (y (+ x 3))) (+ x y))"
  , "((lambda (x y z) (+ x y z)) 1 2 3)"
  , "(fst (1, 2))"
  , "(snd (3, 5))"
  , "(= (+ 1 1) 2 (/ 4 2))"
  , "(< 2 3 7)"
  , "(* 2 (expt 3 2) (sqrt 9))"
  , "(head [1,2,3,4])"
  , "(tail [2,4,6,8])"
  , "(if0 (- 1 1) (>= 2 2) (<= 1 3))"
  , "(if (> 3 4 7) (!= 1 0) #t)"
  ]

-- Función para procesar un ejemplo
processExample :: String -> IO ()
processExample src = do
  putStrLn "===================================="
  putStrLn $ "Expresión: " ++ src
  putStrLn "\n==> Etapa 1: Lexer"
  let toks = Lexer.lexer src
  print toks

  putStrLn "\n==> Etapa 2: Parser"
  let parsed = Grammars.parse toks
  print parsed

  putStrLn "\n==> Etapa 3: Desugar"
  let core = desugar parsed
  print core

  putStrLn "\n==> Etapa 4: Evaluación (Interp)"
  let result = evalFinal (core, [])
  print result

  putStrLn "\n==> Resultado final:"
  print (fst result)
  putStrLn "====================================\n"

-- | Función principal
main :: IO ()
main = mapM_ processExample examples

