{
module BitGridTokens where
}

%wrapper "posn"
$digit = 0-9        --digits
$alpha = [a-zA-Z]   --alphabet

tokens :-
$white+        ;
"//".*         ;

\=             { tok (\p s -> TokenAssignment p) }
\,             { tok (\p s -> TokenComma p) }
".."           { tok (\p s -> TokenRange p) }
\;             { tok (\p s -> TokenEndStmt p) }
\"             { tok (\p s -> TokenQuotation p) }

--comparisons
\<=            { tok (\p s -> TokenLessThanEqual p) }
\>=            { tok (\p s -> TokenGreaterThanEqual p) }
\<             { tok (\p s -> TokenLessThan p) }
\>             { tok (\p s -> TokenGreaterThan p) }
\==            { tok (\p s -> TokenEquals p) }
\!=            { tok (\p s -> TokenNotEquals p) }
\+             { tok (\p s -> TokenPlus p) }
\-             { tok (\p s -> TokenMinus p) }
\*             { tok (\p s -> TokenMultiplication p) }
\/             { tok (\p s -> TokenDivision p) }

--conditions
\?             { tok (\p s -> TokenThen p) }
\:             { tok (\p s -> TokenElse p) }
"&&"           { tok (\p s -> TokenBooleanAnd p) }
"||"           { tok (\p s -> TokenBooleanOr p) }
true           { tok (\p s -> TokenTrue p) }
false          { tok (\p s -> TokenFalse p) }

--types
Int            { tok (\p s -> TokenTypeInt p) }
Tile           { tok (\p s -> TokenTypeTile p) }
Bool           { tok (\p s -> TokenTypeBool p) }
Range          { tok (\p s -> TokenTypeRange p) }

--brackets
\(             { tok (\p s -> TokenLParen p) }
\)             { tok (\p s -> TokenRParen p) }
\[             { tok (\p s -> TokenLBracket p) }
\]             { tok (\p s -> TokenRBracket p) }

--language defined functions
hRepeat        { tok (\p s -> TokenHRepeat p) }
vRepeat        { tok (\p s -> TokenVRepeat p) }
hAdd           { tok (\p s -> TokenHAdd p) }
vAdd           { tok (\p s -> TokenVAdd p) }
rot90          { tok (\p s -> TokenRot90 p) }
rot180         { tok (\p s -> TokenRot180 p) }
rot270         { tok (\p s -> TokenRot270 p) }
grow           { tok (\p s -> TokenGrow p) }
hReflect       { tok (\p s -> TokenHReflect p) }
vReflect       { tok (\p s -> TokenVReflect p) }
blank          { tok (\p s -> TokenBlank p) }
and            { tok (\p s -> TokenAnd p) }
not            { tok (\p s -> TokenNot p) }
or             { tok (\p s -> TokenOr p) }
subtile        { tok (\p s -> TokenSubtile p) }
read           { tok (\p s -> TokenRead p) }

--output holding var
final          { tok (\p s -> TokenFinal p) }

$digit+                                 { tok (\p s -> TokenInt p (read s)) }
$alpha [$alpha $digit \_ \’ \-]*        { tok (\p s -> TokenVar p s) }
$alpha [$alpha $digit \_ \’ \- \.]*     { tok (\p s -> TokenString p s) }

{
-- Helper function
tok f p s = f p s

data TileToken =
    TokenInt AlexPosn Int           |
    TokenVar AlexPosn String        |
    TokenString AlexPosn String     |

    TokenAssignment AlexPosn        |
    TokenComma AlexPosn             |
    TokenRange AlexPosn             |
    TokenEndStmt AlexPosn           |
    TokenQuotation AlexPosn         |

    TokenLessThan AlexPosn          |
    TokenGreaterThan AlexPosn       |
    TokenLessThanEqual AlexPosn     |
    TokenGreaterThanEqual AlexPosn  |
    TokenEquals AlexPosn            |
    TokenNotEquals AlexPosn         |
    TokenPlus AlexPosn              |
    TokenMinus AlexPosn             |
    TokenMultiplication AlexPosn    |
    TokenDivision AlexPosn          |

    TokenThen AlexPosn              |
    TokenElse AlexPosn              |
    TokenBooleanAnd AlexPosn        |
    TokenBooleanOr AlexPosn         |
    TokenTrue AlexPosn              |
    TokenFalse AlexPosn             |

    TokenTypeInt AlexPosn           |
    TokenTypeTile AlexPosn          |
    TokenTypeBool AlexPosn          |
    TokenTypeRange AlexPosn         |

    TokenLParen AlexPosn            |
    TokenRParen AlexPosn            |
    TokenLBracket AlexPosn          |
    TokenRBracket AlexPosn          |

    TokenHRepeat AlexPosn           |
    TokenVRepeat AlexPosn           |
    TokenHAdd AlexPosn              |
    TokenVAdd AlexPosn              |
    TokenRot90 AlexPosn             |
    TokenRot180 AlexPosn            |
    TokenRot270 AlexPosn            |
    TokenGrow AlexPosn              |
    TokenHReflect AlexPosn          |
    TokenVReflect AlexPosn          |
    TokenBlank AlexPosn             |
    TokenAnd AlexPosn               |
    TokenNot AlexPosn               |
    TokenOr AlexPosn                |
    TokenSubtile AlexPosn           |
    TokenRead AlexPosn              |

    TokenFinal AlexPosn
    deriving (Eq,Show)

tokenPosn :: TileToken -> String
tokenPosn (TokenInt (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenAssignment (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRange (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndStmt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenQuotation (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenLessThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThanEqual (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThanEqual (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiplication (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivision (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBooleanAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBooleanOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenTypeInt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeTile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeRange (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenHRepeat (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVRepeat (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHAdd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVAdd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRot90 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRot180 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRot270 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGrow (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenHReflect (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVReflect (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBlank (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubtile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRead (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenFinal (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
}
