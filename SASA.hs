module SASA where

-- Definimos el tipo de listas de SASA
type LSASA = [SASA]

-- Declaramos el tipo de dato que representa nuestra expresión pasada por el analizador gramatical
data SASA
  = NumS Int
  | BoolS Bool
  | IDS String
  | NilS
  | AddS LSASA
  | SubS LSASA
  | MultS LSASA
  | DivS LSASA
  | EqualsS LSASA
  | SmallerS LSASA
  | BiggerS LSASA
  | BigEq LSASA
  | SmallEq LSASA
  | NotEq LSASA
  | Add1 SASA
  | Sub1 SASA
  | SqrtS SASA
  | Expt SASA SASA
  | NotS SASA
  | FstS SASA
  | SndS SASA
  | List LSASA
  | HeadS SASA
  | TailS SASA
  | PairS SASA SASA
  | LetS [Bind] SASA
  | LetSeq [Bind] SASA
  | LetRec Bind SASA
  | IfS SASA SASA SASA
  | If0 SASA SASA SASA
  | LambdaS [String] SASA
  | AppS SASA SASA
  | Cond [Clause] ElseClause
  deriving (Show, Eq)

data Bind = Bind String SASA
  deriving (Show, Eq)

data Clause = Clause SASA SASA
  deriving (Show, Eq)

data ElseClause = Else SASA
  deriving (Show, Eq)
