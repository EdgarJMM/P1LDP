{
module Grammars where
import Lexer (Token(..), lexer)
import ASA
}

%name parse
%tokentype { Token }
%error { parseError }

-- Deifinimos los tokens que nuestro parser va a recibir
%token
    '('               { TokenPA }
    ')'               { TokenPC }
    '+'               { TokenSuma }
    '-'               { TokenResta }
    '*'               { TokenMult }
    '/'               { TokenDiv }

    add1              { TokenAdd1 }
    sub1              { TokenSub1 }
    sqrt              { TokenSqrt }
    expt              { TokenExpt }
    not               { TokenNot }

    let               { TokenLet }
    '='               { TokenIgualdad }
    '<'               { TokenMenorQue }
    '>'               { TokenMayorQue }
    '<='              { TokenMenorIgual }
    '>='              { TokenMayorIgual }
    '!='              { TokenNoIgual }

    fst               { TokenPrimerE }
    snd               { TokenSegundoE }
    "let*"            { TokenLetMultVar }
    letrec            { TokenLetRec }

    if                { TokenIf }
    else              { TokenElse }
    if0               { TokenIf0 }
    lambda            { TokenLambda }

    '['               { TokenCA }
    ']'               { TokenCC }
    ','               { TokenComa }
    head              { TokenHead }
    tail              { TokenTail }
    cond              { TokenCond }
    nil               { TokenNil }

    bool              { TokenBool $$ }
    int               { TokenNum $$ }
    var               { TokenId $$ }

%%

-- Ahora definimos nuestras reglas gramaticales 
SASA : int                             { NumS $1 }
    | bool                            { BoolS $1 }
    | var                             { IDS $1 }
    | nil                             { NilS }

    -- Reglas que pueden recibir una aridad mayor o igual a 2
    | '(' '+' ExprList ')'            { AddS $3 }
    | '(' '-' ExprList ')'            { SubS $3 }
    | '(' '*' ExprList ')'            { MultS $3 }
    | '(' '/' ExprList ')'            { DivS $3 }
    | '(' '=' ExprList ')'            { EqualsS $3 }
    | '(' '<' ExprList ')'            { SmallerS $3 }
    | '(' '>' ExprList ')'            { BiggerS $3 }
    | '(' '<=' ExprList ')'           { BigEq $3 }
    | '(' '>=' ExprList ')'           { SmallEq $3 }
    | '(' '!=' ExprList ')'           { NotEq $3 }

    -- Unarias
    | '(' add1 SASA ')'                { Add1 $3 }
    | '(' sub1 SASA ')'                { Sub1 $3 }
    | '(' sqrt SASA ')'                { SqrtS $3 }
    | '(' expt SASA SASA ')'            { Expt $3 $4 }
    | '(' not SASA ')'                 { NotS $3 }
    | '(' fst SASA ')'                 { FstS $3 }
    | '(' snd SASA ')'                 { SndS $3 }
    | '[' ListElems ']'               { List $2 }
    | '(' head SASA ')'                { Head $3 }
    | '(' tail SASA ')'                { Tail $3 }

    | '(' SASA ',' SASA ')'             { PairS $2 $4 }
    | '(' let BindList SASA ')'        { LetS $3 $4 }
    | '(' "let*" BindList SASA ')'     { LetSeq $3 $4 }
    | '(' letrec BindLet SASA ')'      { LetRec $3 $4 }

    -- condicionales
    | '(' if SASA SASA SASA ')'          { IfS $3 $4 $5 }
    | '(' if0 SASA SASA SASA ')'         { If0 $3 $4 $5 }

    | '(' lambda '(' VarList ')' SASA ')'  { LambdaS $4 $6 }
    | '(' SASA ExprList ')'                { foldl1 AppS ($2 : $3) }
    | '(' cond ClauseList ElseClause  ')' { Cond $3 $4 }

-- Definimos nuestras reglas auxiliares para simplificar las reglas con aridad mayor o igual a 2 
ExprList :
      SASA                { [$1] }
    | SASA ExprList       { $1 : $2 }

BindList :
      BindLet            { [$1] }
    | '(' BindLet BindList ')'  { $2 : $3 }

BindLet :
      '(' var SASA ')'     { Bind $2 $3 }

VarList :
      var                 { [$1] }
    | var VarList         { $1 : $2 }

ListElems :
      SASA                { [$1] }
    | SASA ',' ListElems  { $1 : $3 }

ClauseList :
      Clause             { [$1] }
    | '(' Clause ClauseList ')' { $2 : $3 }

Clause :
      '[' SASA SASA ']'    { Clause $2 $3 }

ElseClause :
      '[' else SASA ']'   { Else $3 }

{

-- Damos la función de error que definimos anteriormente
parseError :: [Token] -> a
parseError _ = error "Parse error"


}
