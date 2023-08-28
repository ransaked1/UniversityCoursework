{
module Tokens (Token(..), AlexPosn(..), alexScanTokens, tokenPosn) where
}

%wrapper "posn"
$digit = 0-9 --digits
$alpha = [a-zA-Z] --alphabetic characters

tokens :-
    $white+     ;
    "--".*      ;
    Bool        { tok (\p s -> TokenTypeBool p) }
    Int         { tok (\p s -> TokenTypeInt p) }
    "->"        { tok (\p s -> TokenFuncType p) }
    \\          { tok (\p s -> TokenLam p) }
    $digit+     { tok (\p s -> TokenInt p (read s)) }
    true        { tok (\p s -> TokenTrue p) }
    false       { tok (\p s -> TokenFalse p) }
    \<          { tok (\p s -> TokenLessThan p) }
    \+          { tok (\p s -> TokenPlus p) }
    if          { tok (\p s -> TokenIf p) }
    then        { tok (\p s -> TokenThen p) }
    else        { tok (\p s -> TokenElse p) }
    let         { tok (\p s -> TokenLet p) }
    in          { tok (\p s -> TokenIn p) }
    \=          { tok (\p s -> TokenEqual p) }
    \(          { tok (\p s -> TokenLParen p) }
    \)          { tok (\p s -> TokenRParen p) }
    \:          { tok (\p s -> TokenColon p) }
    $alpha [$alpha $digit \_ \â€™]*   { tok (\p s -> TokenVar p s) }

{
-- Some action helpers:
tok f p s = f p s

-- Each action has type :: AlexPosn -> String -> Token
-- The token type:
data Token =
  TokenTypeBool AlexPosn    |
  TokenTypeInt AlexPosn     |
  TokenFuncType AlexPosn    |
  TokenLam AlexPosn         |
  TokenInt AlexPosn Int     |
  TokenTrue AlexPosn        |
  TokenFalse AlexPosn       |
  TokenLessThan AlexPosn    |
  TokenPlus AlexPosn        |
  TokenIf AlexPosn          |
  TokenThen AlexPosn        |
  TokenElse AlexPosn        |
  TokenLet AlexPosn         |
  TokenIn AlexPosn          |
  TokenEqual AlexPosn       |
  TokenLParen AlexPosn      |
  TokenRParen AlexPosn      |
  TokenColon AlexPosn       |
  TokenVar AlexPosn String
  deriving (Eq,Show)

tokenPosn (TokenTypeBool (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenTypeInt (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenFuncType (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenLam (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenInt (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenTrue (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenFalse (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenLessThan (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenPlus (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenIf (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenThen (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenElse (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenLet (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenIn (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenEqual (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenLParen (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenRParen (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenColon (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenVar (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
}