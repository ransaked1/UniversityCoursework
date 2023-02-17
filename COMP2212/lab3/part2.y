{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }
%token
    let { TokenLet $$ }
    in  { TokenIn $$ }
    int { TokenInt i $$ }
    var { TokenVar i $$ }
    '=' { TokenEq x $$}
    '+' { TokenPlus x $$}
    '-' { TokenMinus x $$ }
    '*' { TokenTimes x $$ }
    '/' { TokenDiv x $$ }
    '(' { TokenLParen x $$ }
    ')' { TokenRParen x $$ }
    '^' { TokenExp x $$ }

%right in
%left '+' '-'
%left '*' '/'
%left '^'
%left NEG
%%
Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
    | Exp '+' Exp            { Plus $1 $3 }
    | Exp '-' Exp            { Minus $1 $3 }
    | Exp '*' Exp            { Times $1 $3 }
    | Exp '/' Exp            { Div $1 $3 }
    | Exp '^' Exp            { Expn $1 $3}
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Negate $2 }
    | int                    { Int $1 }
    | var                    { Var $1 }

{
parseError :: [Token] -> a
parseError xs = error ("Parse error at " ++ show (tokenPosn (xs !! 0)))

data Exp = Let String Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Expn Exp Exp
         | Negate Exp
         | Int Int
         | Var String
         deriving Show
}