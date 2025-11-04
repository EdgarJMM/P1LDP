module Desugar where

import SASA  -- Importamos el módulo donde definimos nuestro SASA
import qualified Grammars -- Importamos el módulo Grammars para acceder a sus funiones, tipos o valores

-- Definimos nuestro árbol de sitaxis abstracta con las expresiones núcleo
data ASA
  = Id String
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Mult ASA ASA
  | Div ASA ASA
  | Eq ASA ASA
  | MenorQue ASA ASA
  | MayorQue ASA ASA
  | Not ASA
  | If ASA ASA ASA
  | Fun String ASA
  | App ASA ASA
  | Seq [ASA]
  | Pair ASA ASA
  | Fst ASA
  | Snd ASA
  | Nil
  | Cons ASA ASA
  | Head ASA 
  | Tail ASA
  deriving (Eq, Show)

-- Definimos nuestra función desugar que recibe un árbol de sintaxis concreta y devolvemos un árbol de sitaxis abstracta
desugar :: SASA -> ASA

-- Datos que ya son núcleo y por lo tanto se mantienen 
desugar (NumS n)      = Num n
desugar (BoolS b)     = Boolean b
desugar (IDS x)       = Id x
desugar NilS          = Nil
-- Desazucarización de las operaciones con aridad mayor o igual a 2
desugar (AddS xs)     = foldl1 Add (map desugar xs)
desugar (SubS xs)     = foldl1 Sub (map desugar xs)
desugar (MultS xs)    = foldl1 Mult (map desugar xs)
desugar (DivS xs)     = foldl1 Div (map desugar xs)

-- Comparaciones con aridad mayor o igual a 2
desugar (EqualsS xs)  = chainComparison Eq xs
desugar (BiggerS xs)  = chainComparison MayorQue xs
desugar (SmallerS xs) = chainComparison MenorQue xs
desugar (BigEq xs)   = chainComparison auxBE xs
desugar (SmallEq xs) = chainComparison auxSE xs
desugar (NotEq xs) = chainComparison auxNE xs

-- Desazucarización de expresiones unarias
desugar (Add1 e)      = Add (desugar e) (Num 1)
desugar (Sub1 e)      = Sub (desugar e) (Num 1)
desugar (SqrtS e)     = App (Id "sqrt") (desugar e)
desugar (Expt a b)    = App (App (Id "expt") (desugar a)) (desugar b)
desugar (NotS e)      = Not (desugar e)
desugar (PairS a b)   = Pair (desugar a) (desugar b)
desugar (FstS e)      = Fst (desugar e)
desugar (SndS e)      = Snd (desugar e)
desugar (List xs)     = foldr Cons Nil (map desugar xs)
desugar (HeadS e)      = Head (desugar e)
desugar (TailS e)      = Tail (desugar e)
-- Desazucarización de funciones, aplicación y condicionales
desugar (LambdaS ps b) = curryFun ps (desugar b)
desugar (AppS f a)     = App (desugar f) (desugar a)
desugar (IfS c t e)    = If (desugar c) (desugar t) (desugar e)
desugar (If0 c t e)    = If (Eq (desugar c) (Num 0)) (desugar t) (desugar e)

-- Let bindings
-- Desazucarización de let paralelo
-- (λx1 x2 ... xn. body) e1 e2 ... en
desugar (LetS binds body) =
  let vars  = [x | Bind x _ <- binds]
      exprs = [e | Bind _ e <- binds]
      fun   = curryFun vars (desugar body)
  in  foldl App fun (map desugar exprs)
-- Desazucarización de let secuencial como aplicación lambda anidada
-- letseq x1 = e1; x2 = e2; ...; xn = en in body
-- ≡ ((λx1. (λx2. ... (λxn. body) e_n ...) e2) e1)
desugar (LetSeq [] body) = desugar body
desugar (LetSeq (Bind x e : bs) body) =
  App (Fun x (desugar (LetSeq bs body))) (desugar e)
  
desugar (LetRec (Bind f e1) e2) =
  App (Fun f (desugar e2))
      (App (Id "fix") (Fun f (desugar e1)))

-- Desazucarización de la condicional
desugar (Cond clauses (Else e)) =
  foldr (\(Clause cond expr) acc -> If (desugar cond) (desugar expr) acc)
        (desugar e)
        clauses

-- Mensaje de error en caso de no haber una implementación
desugar e = error $ "Desugar aún no implementado para: " ++ show e

-- función auxiliar para descomponer BigEq en Bigger, Eq e if
auxBE :: ASA -> ASA -> ASA
auxBE a b = If (MayorQue a b) (Boolean True) (Eq a b)

-- función auxiliar para descomponer SmallEq en Smaller, Eq e if
auxSE :: ASA -> ASA -> ASA
auxSE a b = If (MenorQue a b) (Boolean True) (Eq a b)

-- función auxiliar para descomponer NotEq en Not, Eq e if
auxNE :: ASA -> ASA -> ASA
auxNE a b = Not (Eq a b)

-- Función auxiliar para facilitar las comparaciones son aridad mayor a 2
chainComparison :: (ASA -> ASA -> ASA) -> [SASA] -> ASA
chainComparison _ []  = Boolean True
chainComparison _ [_] = Boolean True
chainComparison ctor (x:y:rest) =
  let c1 = ctor (desugar x) (desugar y)   -- Primera comparación a > b
      c2 = chainComparison ctor (y:rest)  -- Comparaciones siguientes
  in  If c1 c2 (Boolean False)

-- Función auxiliar que transforma un SASA LambdaS a un ASA Fun currificado
curryFun :: [String] -> ASA -> ASA
curryFun [] body     = body
curryFun (p:ps) body = Fun p (curryFun ps body)
