{
module Lexer (Token(..), lexer) where
import Data.Char (isSpace)
}
-- indicamos el tipo de envoltorio básico
%wrapper "basic"

--Indicamos nuestros dígitos y alfabeto
$digit   = 0-9
$letter  = [A-Za-z]
$idrest  = [A-Za-z0-9_]
--Indicamos en código hexadecimal los posibles espacios en blanco que pueden aparecer que queremos ignorar
--Donde cada uno significa \x20 = ' ' (space), \x09 = tab, \x0A = LF, \x0D = CR, \x0C = FF, \x0B = VT
$espB    = [\x20\x09\x0A\x0D\x0C\x0B]

-- Fin de macros e inicio de tokenización
tokens :-

-- Como definimos nuestro tokens
$espB+                  ;
"~~".*                  ;
\(                      { \_ -> TokenPA }
\)                      { \_ -> TokenPC }
\+                      { \_ -> TokenSuma }
\-                      { \_ -> TokenResta }
\*                      { \_ -> TokenMult }
\/                      { \_ -> TokenDiv }
add1                    { \_ -> TokenAdd1 }
sub1                    { \_ -> TokenSub1 }
sqrt                    { \_ -> TokenSqrt }
expt                    { \_ -> TokenExpt }
not                     { \_ -> TokenNot }
let                     { \_ -> TokenLet }
\=                      { \_ -> TokenIgualdad }
\<                      { \_ -> TokenMenorQue }
\>                      { \_ -> TokenMayorQue }
\<\=                    { \_ -> TokenMenorIgual }
\>\=                    { \_ -> TokenMayorIgual }
\!\=                    { \_ -> TokenNoIgual }
fst                     { \_ -> TokenPrimerE }
snd                     { \_ -> TokenSegundoE }
let\*                   { \_ -> TokenLetMultVar }
letrec                  { \_ -> TokenLetRec }
if                      { \_ -> TokenIf }
else                    { \_ -> TokenElse }
if0                     { \_ -> TokenIf0 }
lambda                  { \_ -> TokenLambda }
nil                     { \_ -> TokenNil }
\[                      { \_ -> TokenCA }
\]                      { \_ -> TokenCC }
\,                      { \_ -> TokenComa }
head                    { \_ -> TokenHead }
tail                    { \_ -> TokenTail }
cond                    { \_ -> TokenCond }

"#t"                    { \_ -> TokenBool True }
"#f"                    { \_ -> TokenBool False }

$digit+                 { \s -> TokenNum (read s) }
$letter$idrest*         { \s -> TokenId s }


-- Catch-all para diagnosticar caracteres inesperados
.                       { \s -> error ("Error léxico: carácter no reconocido = "
                                      ++ show s
                                      ++ " | codepoints = "
                                      ++ show (map fromEnum s)) }
-- Código en haskell que define los tokens
{
data Token
  = TokenId String
  | TokenNum Int
  | TokenBool Bool
  | TokenSuma
  | TokenResta
  | TokenMult
  | TokenDiv
  | TokenAdd1
  | TokenSub1
  | TokenSqrt
  | TokenExpt
  | TokenNot
  | TokenLet
  | TokenLetMultVar
  | TokenLetRec
  | TokenIf
  | TokenIf0
  | TokenLambda
  | TokenIgualdad
  | TokenMenorQue
  | TokenMayorQue
  | TokenMenorIgual
  | TokenMayorIgual
  | TokenNoIgual
  | TokenPrimerE
  | TokenSegundoE
  | TokenCond
  | TokenNil
  | TokenHead
  | TokenTail
  | TokenComa
  | TokenCA
  | TokenCC
  | TokenPA
  | TokenPC
  | TokenElse
  deriving (Show, Eq)


-- Normaliza cualquier espacios en blanco Unicode a ' ' para que $white+ lo consuma
normalizeSpaces :: String -> String
normalizeSpaces = map (\c -> if isSpace c then '\x20' else c)

-- Alias: Alex define alexScanTokens (String -> [Token])
lexer :: String -> [Token]
lexer = alexScanTokens . normalizeSpaces
}
