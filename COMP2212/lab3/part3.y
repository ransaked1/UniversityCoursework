{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }
%token
    '{' { TokenLBrack $$ }
    '}'  { TokenRBrack $$ }
    ',' { TokenComma $$ }
    move { TokenMove $$ }
    int { TokenInt x $$}
    digit { TokenCheckInt x $$}
    rotl { TokenRotL $$ }
    rotr { TokenRotR $$ }
    if { TokenIf $$ }
    check { TokenCheck $$ }
    '(' { TokenLParen $$ }
    ')' { TokenRParen $$ }
    then { TokenThen $$ }
    else { TokenElse $$ }

%%
Block : '{' Actions '}'             { Block $2 }
      | '{' Actions '}' ',' Block   { BlockComma $2 $5 }

Actions : move int                  { MoveInt $2 }
        | move digit                { MoveDigit $2 }
        | rotr                      { RotR }
        | rotl                      { RotL }
        | Condition                 { Cond $1 }
        | move int ',' Actions      { MoveIntComma $2 $4 }
        | move digit ',' Actions    { MoveDigitComma $2 $4 }
        | rotr ',' Actions          { RotRComma $3 }
        | rotl ',' Actions          { RotLComma $3 }
        | Condition ',' Actions     { CondComma $1 $3 }

Condition : if '(' check digit ')' then '{' Actions '}'                           {ConditionIfThen $4 $8}
          | if '(' check digit ')' then '{' Actions '}' else '{' Actions '}'      {ConditionIfThenElse $4 $8 $12}
{
parseError :: [Token] -> a
parseError xs = error ("Parse error at " ++ show (tokenPosn (xs !! 0)) ++ show (take 5 xs))

data Block = Block Actions | BlockComma Actions Block deriving Show

data Actions = MoveInt Int
             | MoveDigit Int
             | RotR
             | RotL
             | Cond Condition
             | MoveIntComma Int Actions
             | MoveDigitComma Int Actions
             | RotRComma Actions
             | RotLComma Actions
             | CondComma Condition Actions
             deriving Show

data Condition = ConditionIfThen Int Actions
               | ConditionIfThenElse Int Actions Actions deriving Show
}