{
module Tokens (Token(..), AlexPosn(..), alexScanTokens, tokenPosn) where
}

%wrapper "posn"
$digit = 0-9 --digits
$alpha = [a-zA-Z] --alphabetic characters

tokens :-
    $white+     ;
    "--".*      ;
    \{          { tok (\p s -> TokenLBrack p) }
    \}          { tok (\p s -> TokenRBrack p) }
    \,          { tok (\p s -> TokenComma p) }
    move        { tok (\p s -> TokenMove p) }
    $digit+     { tok (\p s -> TokenInt p (read s)) }
    [1-9]{1}    { tok (\p s -> TokenCheckInt p (read s)) }
    rotl        { tok (\p s -> TokenRotL p) }
    rotr        { tok (\p s -> TokenRotR p) }
    if          { tok (\p s -> TokenIf p) }
    check       { tok (\p s -> TokenCheck p) }
    \(          { tok (\p s -> TokenLParen p) }
    \)          { tok (\p s -> TokenRParen p) }
    then        { tok (\p s -> TokenThen p) }
    else        { tok (\p s -> TokenElse p) }

{
-- Some action helpers:
tok f p s = f p s

-- Each action has type :: AlexPosn -> String -> Token
-- The token type:
data Token =
  TokenLBrack AlexPosn          |
  TokenRBrack AlexPosn          |
  TokenComma AlexPosn           |
  TokenMove AlexPosn            |
  TokenInt AlexPosn Int         |
  TokenCheckInt AlexPosn Int    |
  TokenRotL AlexPosn            |
  TokenRotR AlexPosn            |
  TokenIf AlexPosn              |
  TokenCheck AlexPosn           |
  TokenLParen AlexPosn          |
  TokenRParen AlexPosn          |
  TokenThen AlexPosn            |
  TokenElse AlexPosn
  deriving (Eq,Show)

tokenPosn (TokenLBrack (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenRBrack (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenComma (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenMove (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenInt (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenCheckInt (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenRotL (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenRotR (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenIf (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenCheck (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenLParen (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenRParen (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenThen (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenElse (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
}