module MenuInteractivo where

import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
-- Llamamos nuestros módulos 
import Lexer (lexer)
import Grammars (parse)   
import Desugar (desugar)
import Interp (evalFinal)

-- Función que evalúa la entrada paso a paso hasta llegar al resultado
run :: String -> IO ()
run code = do
    let tokens = lexer code
        sasa = parse tokens
        asa = desugar sasa
        (valor, _) = evalFinal (asa, [])
    putStrLn $ "\n" ++ show valor ++ "\n"

-- Bienvenida del menú interactivo
main :: IO ()
main = do
    putStrLn "=============================="
    putStrLn "|    Intérprete MiniLisp     |"
    putStrLn "=============================="
    putStrLn ":q para salir del intérprete" 
    runInputT defaultSettings itera

itera :: InputT IO ()
itera = do
    minput <- getInputLine "UMCi> "
    case minput of
        Nothing -> outputStrLn "Terminando ejecución..."
        Just ":q" -> outputStrLn "Terminando ejecución..."
        -- Sino se escribe nada seguimos interando
        Just ""   -> itera
        -- verificamos que no ocurran excepciones 
        Just code -> do
            liftIO $ safeRun code
            itera

-- Captura errores léxicos, sintácticos o de evaluación
safeRun :: String -> IO ()
safeRun code =
    (run code)
    `catch` (\e -> do
        putStrLn $ "Error: " ++ show (e :: SomeException)
    )

