{
module Tokens (Token(..), AlexPosn(..), alexScanTokens, tokenPosn) where
}

%wrapper "posn"
$digit = 0-9 --digits
$alpha = [a-zA-Z] --alphabetic characters

tokens :-
    $white+     ;
    "--".*      ;
    let         { tok (\p s -> TokenLet p) }
    in          { tok (\p s -> TokenIn p) }
    $digit+     { tok (\p s -> TokenInt p (read s)) }
    \=          { tok (\p s -> TokenEq p (head s)) }
    \+          { tok (\p s -> TokenPlus p (head s)) }
    \-          { tok (\p s -> TokenMinus p (head s)) }
    \*          { tok (\p s -> TokenTimes p (head s)) }
    \/          { tok (\p s -> TokenDiv p (head s)) }
    \^          { tok (\p s -> TokenExp p (head s)) }
    \(          { tok (\p s -> TokenLParen p (head s)) }
    \)          { tok (\p s -> TokenRParen p (head s)) }
    $alpha [$alpha $digit \_ \â€™]*   { tok (\p s -> TokenVar p s) }

{
-- Some action helpers:
tok f p s = f p s

-- Each action has type :: AlexPosn -> String -> Token
-- The token type:
data Token =
  TokenLet AlexPosn        |
  TokenIn AlexPosn         |
  TokenInt AlexPosn Int    |
  TokenVar AlexPosn String |
  TokenEq AlexPosn Char        |
  TokenPlus AlexPosn Char      |
  TokenMinus AlexPosn Char     |
  TokenTimes AlexPosn Char     |
  TokenDiv AlexPosn Char       |
  TokenLParen AlexPosn Char    |
  TokenRParen AlexPosn Char    |
  TokenExp AlexPosn Char
  deriving (Eq,Show)

tokenPosn (TokenLet (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenIn (AlexPn _ line col)) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenInt (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenVar (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenEq (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenPlus (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenMinus (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenTimes (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenDiv (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenExp (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenLParen (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
tokenPosn (TokenRParen (AlexPn _ line col) _) = show(line) ++ ":" ++ show(col)
}