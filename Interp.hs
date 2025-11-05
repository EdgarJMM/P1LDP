module Interp where

import Desugar -- Importamos el módulo con el ASA

-- Sintaxis ASAV
data ASAV = NumV Int
              | BooleanV Bool
              | PairV ASAV ASAV
              | ConsV ASAV ASAV   -- agregado para listas
              | NilV              -- agregado para lista vacía
              | Closure String ASA ENV
              | Error String
              | AppV ASAV ASA
              deriving (Show)

-- Tipo de dato ENV que representa a los ambientes léxicos.
-- Un ambiente léxico se puede representar como una lista de
-- tuplas de identificadores (String) con un valor canónico.
type ENV = [(String, ASAV)]

-- Configuraciones (tuplas) de ASA, ENV
type Config = (ASA, ENV)

-- Función de transición de paso pequeño (small step)
smallStep :: Config -> Config
smallStep (Nil, env) = (Nil, env)                                                 -- Nil-Rule
smallStep ((Id x), env) =                                                         -- Lookup-Hit
                    let v = lookupHit x env  
                    in (returnASA(v), env)                       
smallStep ((Num n), env) = ((Num n), env)                                        -- Num-Rule
smallStep ((Boolean b), env) = ((Boolean b), env)                                -- Boolean-Rule
smallStep ((Add (Num n) (Num m)), env) = (Num (n + m), env)                    -- Add-Num
smallStep ((Add (Num n) e2), env) = 
                                    let (e2', env') = smallStep (e2, env)
                                    in (Add (Num n) e2', env')                       -- Add-Right
smallStep ((Add e1 e2), env) = 
                            let (e1', env') = smallStep (e1, env)          
                            in ((Add e1' e2), env')                                   -- Add-Left
smallStep ((Sub (Num n) (Num m)), env) = (Num (n - m), env)                    -- Sub-Num
smallStep ((Sub (Num n) e2), env) = 
                                let (e2', env') = smallStep (e2, env)
                                in (Sub (Num n) e2', env')                           -- Sub-Right
smallStep ((Sub e1 e2), env) = 
                            let (e1', env') = smallStep (e1, env)          
                            in ((Sub e1' e2), env')                                   -- Sub-Left
smallStep ((Mult (Num n) (Num m)), env) = (Num (n * m), env)                   -- Mult-Num
smallStep ((Mult (Num n) e2), env) = 
                                    let (e2', env') = smallStep (e2, env)
                                    in (Mult (Num n) e2', env')                      -- Mult-Right
smallStep ((Mult e1 e2), env) = 
                            let (e1', env') = smallStep (e1, env)          
                            in ((Mult e1' e2), env')                                  -- Mult-Left
smallStep ((Div (Num n) (Num 0)), env) = error ("Error. División entre cero detectada")  -- Div-Cero
smallStep ((Div (Num n) (Num m)), env) = (Num (div n m), env)                   -- Div-Num
smallStep ((Div (Num n) e2), env) = 
                                    let (e2', env') = smallStep (e2, env)
                                    in (Div (Num n) e2', env')                       -- Div-Right
smallStep ((Div e1 e2), env) = 
                            let (e1', env') = smallStep (e1, env)          
                            in ((Div e1' e2), env')                                   -- Div-Left
smallStep ((Eq (Num n) (Num m)), env) = (Boolean (n == m), env)                  -- Eq-Num
smallStep ((Eq (Num n) e2), env) =                                               -- Eq-Right
                                    let (e2', env') = smallStep (e2, env)
                                    in ((Eq (Num n) e2'), env')
smallStep ((Eq e1 e2), env) =                                                   -- Eq-Left
                                    let (e1', env') = smallStep (e1, env)
                                    in ((Eq e1' e2), env')
smallStep ((MayorQue (Num n) (Num m)), env) = (Boolean (n > m), env)             -- MayorQue-Num
smallStep ((MayorQue (Num n) e2), env) =                                         -- MayorQue-Right
                                    let (e2', env') = smallStep (e2, env)
                                    in ((MayorQue (Num n) e2'), env')
smallStep ((MayorQue e1 e2), env) =                                               -- MayorQue-Left
                                    let (e1', env') = smallStep (e1, env)
                                    in ((MayorQue e1' e2), env')
smallStep ((MenorQue (Num n) (Num m)), env) = (Boolean (n < m), env)             -- MenorQue-Num
smallStep ((MenorQue (Num n) e2), env) =                                         -- MenorQue-Right
                                    let (e2', env') = smallStep (e2, env)
                                    in ((MenorQue (Num n) e2'), env')
smallStep ((MenorQue e1 e2), env) =                                               -- MenorQue-Left
                                    let (e1', env') = smallStep (e1, env)
                                    in ((MenorQue e1' e2), env')
smallStep ((Not (Boolean False)), env) = (Boolean True, env)                     -- Not-False
smallStep ((Not (Boolean True)), env) = (Boolean False, env)                     -- Not-True                   
smallStep ((Not e), env) =                                                         -- Not-Step
                            let (e', env') = smallStep (e, env)
                            in (Not e', env')
smallStep ((Pair v1 v2), env)                                                      -- Pair-Val
                    | isCanon v1 && isCanon v2 = ((Pair v1 v2), env)                             
smallStep ((Pair v e2), env)                                                       -- Pair-Right
                    | isCanon v =                                                
                        let (e2', env') = smallStep (e2, env)
                        in (Pair v e2', env')
smallStep ((Pair e1 e2), env) =                                                    -- Pair-Left
                            let (e1', env') = smallStep (e1, env)
                            in (Pair e1' e2, env')
smallStep ((If (Boolean True) t f), env) = (t, env)                               -- If-True
smallStep ((If (Boolean False) t f), env) = (f, env)                              -- If-False
smallStep ((If g t f), env) =                                                      -- If-Guard
                            let (g', env') = smallStep (g, env)
                            in ((If g' t f), env')
smallStep ((Fst (Pair v1 v2)), env) = (v1, env)                                   -- Fst-Pair
smallStep ((Fst v), env)                                                           -- Fst-Err
                | ((isCanon v) && (isPair v) == False) = error ("Error. La operacion fst solo funciona con pares")
smallStep ((Fst e), env) =                                                         -- Fst-Arg
                            let (e', env') = smallStep (e, env)
                            in (Fst e', env')
smallStep ((Snd (Pair v1 v2)), env) = (v2, env)                                   -- Snd-Pair
smallStep ((Snd v), env)                                                           -- Snd-Err
                | ((isCanon v) && (isPair v) == False) = error ("Error. La operacion snd solo funciona con pares") 
smallStep ((Snd e), env) =                                                         -- Snd-Arg
                        let (e', env') = smallStep (e, env)
                        in (Snd e', env')
smallStep ((Cons v1 v2), env)                                                      -- Cons-Val
                    | isCanon v1 && isCanon v2 = ((Cons v1 v2), env)                             
smallStep ((Cons v e2), env)                                                       -- Cons-Right
                    | isCanon v =                                                
                            let (e2', env') = smallStep (e2, env)
                            in (Cons v e2', env')
smallStep ((Cons e1 e2), env) =                                                    -- Cons-Left
                            let (e1', env') = smallStep (e1, env)
                            in (Cons e1' e2, env')
smallStep ((Head (Cons e1 e2)), env) = (e1, env)                                  -- Head-Cons
smallStep ((Head e), env) =                                                       -- Head-Arg
                            let (e', env') = smallStep (e, env)
                            in (Head e', env')
smallStep ((Tail (Cons e1 e2)), env) = (e2, env)                                  -- Tail-Cons
smallStep ((Tail e), env) =                                                       -- Tail-Arg
                            let (e', env') = smallStep (e, env)
                            in (Tail e', env')
-- Primitivas sqrt y expt
smallStep ((App (Id "sqrt") (Num n)), env) = (Num (floor (sqrt (fromIntegral n))), env)
smallStep ((App (Id "sqrt") e2), env) =
    let (e2', env') = smallStep (e2, env)
    in (App (Id "sqrt") e2', env')
smallStep ((App (App (Id "expt") (Num n)) (Num m)), env) = (Num (n ^ m), env)
smallStep ((App (App (Id "expt") (Num n)) e2), env) =
    let (e2', env') = smallStep (e2, env)
    in (App (App (Id "expt") (Num n)) e2', env')
smallStep ((App (Id "expt") e1), env) =
    let (e1', env') = smallStep (e1, env)
    in (App (Id "expt") e1', env')
smallStep ((App (Fun p c) e2), env) =                                           -- App-Bv y App-Arg
                            doClosure (appArg (funToClosure ((App (Fun p c) e2), env)))
smallStep ((App e1 e2), env) =                                                  -- App-Fun
                            let (e1', env') = smallStep (e1, env)
                            in ((App e1' e2), env')
smallStep ((Fun p c), env) = ((Fun p c), env)

-- Tipo de dato de configuraciones de ASAV, ENV
type ConfigASAV = (ASAV, ENV) 

-- Funcion de transición de paso pequeño de una aplicación de función a un aplicación de closure
funToClosure :: Config -> ConfigASAV                        
funToClosure ((App (Fun p c) a), env) = ((AppV (Closure p c env)) a, env)      -- Lam-Clos

-- Funcion de transición de paso pequeño de App-Arg
appArg :: ConfigASAV -> ConfigASAV                        
appArg ((AppV (Closure p c env0) a), env)
    | not (isCanon a) =
      let (aFinal, envFinal) = eval (a, env)
      in ((AppV (Closure p c env0)) aFinal, envFinal)
    | otherwise = ((AppV (Closure p c env0)) a, env)

-- Funcion de transición de paso pequeño de una aplicación de closure a su beta reducción
doClosure :: ConfigASAV -> Config
doClosure ((AppV (Closure p c env0) a), env) = (c, (p, toASAV(a)):env0)         -- App-Bv

-- Funcion auxiliar que verifica si una ASA pertenece al conjunto de estados finales (en su equivalente
-- en ASAV), es decir, es un valor canónico.
isCanon :: ASA -> Bool
isCanon (Num _) = True
isCanon (Boolean _) = True
isCanon (Pair _ _) = True
isCanon (Fun _ _) = True
isCanon (Cons _ _) = True     -- agregado para listas
isCanon Nil = True            -- agregado para lista vacía
isCanon _ = False

-- Funcion auxiliar que verifica si un ASA es un Par
isPair :: ASA -> Bool
isPair (Pair _ _) = True
isPair _ = False

-- Funcion que transforma un ASA canónico en su forma equivalente en ASAV.    
toASAV :: ASA -> ASAV
toASAV (Num n) = (NumV n)
toASAV (Boolean b) = (BooleanV b)
toASAV (Pair l r) = (PairV (toASAV(l)) (toASAV(r)))
toASAV (Fun x c) = Closure x c []  -- Aquí, capturamos el closure con el ambiente vacío
toASAV (Cons l r) = (ConsV (toASAV l) (toASAV r))   -- agregado
toASAV Nil = NilV   -- agregado

toASAV _ = Error "No se puede convertir ASA a ASAV"

-- Funcion que transforma un ASAV en su forma equivalente en ASA.    
returnASA :: ASAV -> ASA
returnASA (NumV n) = (Num n)
returnASA (BooleanV b) = (Boolean b)
returnASA (PairV l r) = (Pair (returnASA(l)) (returnASA(r)))
returnASA (ConsV l r) = (Cons (returnASA l) (returnASA r))   -- agregado
returnASA NilV = Nil  -- agregado
returnASA (Closure x c env) = Fun x c
returnASA (Error msg) = error msg 

-- Funcion que evalúa configuraciones usando smallStep hasta llegar a un valor canónico
eval :: Config -> Config
eval (e, env)
    | isCanon e = (e, env)
    | otherwise = eval (smallStep (e, env))

-- Funcion que evalúa configuraciones usando eval y devuelve un resultado ASAV
evalFinal :: Config -> (ASAV, ENV)
evalFinal (e , env) = (case eval (e, env) of
                        (valor, env) -> (toASAV(valor), env))

-- Funcion que busca el valor de un identificador en un ambiente y lo devuelve
lookupHit :: String -> ENV -> ASAV
lookupHit i [] = Error ("Error LookUpMiss: El identificador " ++ i ++ " es una variable libre")
lookupHit i ((x,v):xs) = if i == x then v else lookupHit i xs 

