{
module BitGridGrammar where
import BitGridTokens
}

%name parseCalc
%tokentype { TileToken }
%error { parseError }

%token
    '='         { TokenAssignment _ }
    ','         { TokenComma _ }
    '..'        { TokenRange _ }
    ';'         { TokenEndStmt _ }
    '"'         { TokenQuotation _ }

    '<='        { TokenLessThanEqual _ }
    '>='        { TokenGreaterThanEqual _ }
    '<'         { TokenLessThan _ }
    '>'         { TokenGreaterThan _ }
    '=='        { TokenEquals _ }
    '!='        { TokenNotEquals _ }
    '+'         { TokenPlus _ }
    '-'         { TokenMinus _ }
    '*'         { TokenMultiplication _ }
    '/'         { TokenDivision _ }

    '?'         { TokenThen _ }
    ':'         { TokenElse _ }
    '&&'        { TokenBooleanAnd _ }
    '||'        { TokenBooleanOr _ }

    Int         { TokenTypeInt _ }
    Tile        { TokenTypeTile _ }
    Bool        { TokenTypeBool _ }
    Range       { TokenTypeRange _ }

    '('         { TokenLParen _ }
    ')'         { TokenRParen _ }
    '['         { TokenLBracket _ }
    ']'         { TokenRBracket _ }

    hRepeat     { TokenHRepeat _ }
    vRepeat     { TokenVRepeat _ }
    hAdd        { TokenHAdd _ }
    vAdd        { TokenVAdd _ }
    rot90       { TokenRot90 _ }
    rot180      { TokenRot180 _ }
    rot270      { TokenRot270 _ }
    grow        { TokenGrow _ }
    hReflect    { TokenHReflect _ }
    vReflect    { TokenVReflect _ }
    blank       { TokenBlank _ }
    and         { TokenAnd _ }
    not         { TokenNot _ }
    or          { TokenOr _ }
    subtile     { TokenSubtile _ }
    read        { TokenRead _ }

    final       { TokenFinal _ }

    int         { TokenInt _ $$ }
    var         { TokenVar _ $$ }
    string      { TokenString _ $$ }
    true        { TokenTrue _ }
    false       { TokenFalse _ }

%nonassoc '<' '>' '<=' '>=' '==' '!='
%nonassoc '=' ',' '..' ';' '"'
%nonassoc '(' ')' '[' ']'
%nonassoc '?' ':'

%left '+' '-' '*' '/'
%left '&&' '||'


%left hRepeat vRepeat hAdd vAdd hReflect vReflect
%left rot90 rot180 rot270
%left grow mirror blank subtile
%left and not or
%left NEG

%%
Program : Type final '=' Vars ';'                  { ProgramSingle $1 $4 }
        | Assignments Type final '=' Vars ';'      { ProgramMultiple $1 $2 $5 }

Assignments : Type var '=' Vars ';'                { AssignmentFinal $1 $2 $4 }
            | Assignments Type var '=' Vars ';'    { AssignmentMultiple $1 $2 $3 $5 }

Vars : Operation      { Operation $1 }
     | Arith          { Arith $1 }
     | BooleanArith   { BooleanArith $1 }
     | RangeArith     { RangeArith $1 }


Arr : '[' Binaries ']'                { TileUnidimensional $2 }
    | '[' '[' Binaries ']' Arrs ']'   { TileBidimensional $3 $5 }

Arrs : ',' '[' Binaries ']'           { TileFinal $3 }
     | ',' '[' Binaries ']' Arrs      { TileMultiple $3 $5 }

Binaries : int ',' Binaries   { IntComma $1 $3}
         | int                { IntBinary $1 }


Operation : '(' Operation ')'                         { $2 }
          | var                                       { Var $1 }
          | Arr                                       { Tile $1 }

          | hRepeat RangeArith Operation              { HRepeatRange $2 $3 }
          | vRepeat RangeArith Operation              { VRepeatRange $2 $3 }
          | hRepeat Arith Operation                   { HRepeat $2 $3 }
          | vRepeat Arith Operation                   { VRepeat $2 $3 }
          | hRepeat Operation Operation               { HRepeatVar $2 $3 }
          | vRepeat Operation Operation               { VRepeatVar $2 $3 }
          | hAdd Operation Operation                  { HAdd $2 $3 }
          | vAdd Operation Operation                  { VAdd $2 $3 }
          | rot90 Operation                           { Rot90 $2 }
          | rot180 Operation                          { Rot180 $2 }
          | rot270 Operation                          { Rot270 $2 }
          | grow Arith Operation                      { Grow $2 $3 }
          | grow Operation Operation                  { GrowVar $2 $3 }
          | hReflect Operation                        { HReflect $2 }
          | vReflect Operation                        { VReflect $2 }
          | blank Arith Arith                         { Blank $2 $3 }
          | blank Arith Operation                     { BlankArithVar $2 $3 }
          | blank Operation Arith                     { BlankVarArith $2 $3 }
          | blank Operation Operation                 { BlankVar $2 $3 }
          | not Operation                             { Not $2 }
          | and Operation Operation                   { And $2 $3 }
          | or Operation Operation                    { Or $2 $3 }
          | subtile Vars Vars Vars Operation          { Subtile $2 $3 $4 $5 }
          | read '"' string '"'                       {Read $3 }

          | '(' BooleanArith ')' '?' '(' Vars ')' ':' '(' Vars ')'  { ConditionThenElse $2 $6 $10 }

BooleanArith : '(' BooleanArith ')'                 { $2 }
             | true                               { BooleanTrue }
             | false                              { BooleanFalse }

             | BooleanArith  '&&'  BooleanArith   { BooleanAnd $1 $3 }
             | BooleanArith  '||'  BooleanArith   { BooleanOr $1 $3 }
             | BooleanArith '&&' Operation        { BooleanAndBoolVar $1 $3 }
             | BooleanArith '||' Operation        { BooleanOrBoolVar $1 $3 }
             | Operation '&&' BooleanArith        { BooleanAndVarBool $1 $3 }
             | Operation '||' BooleanArith        { BooleanOrVarBool $1 $3 }
             | Operation '&&' Operation           { BooleanAndVar $1 $3 }
             | Operation '||' Operation           { BooleanOrVar $1 $3 }

             | Arith '<' Arith                    { BooleanLessThan $1 $3 }
             | Arith '>' Arith                    { BooleanGreaterThan $1 $3 }
             | Arith '<=' Arith                   { BooleanLessThanEqual $1 $3 }
             | Arith '>=' Arith                   { BooleanGreaterThanEqual $1 $3 }
             | Arith '==' Arith                   { BooleanEquals $1 $3 }
             | Arith '!=' Arith                   { BooleanNotEquals $1 $3 }

             | Operation '<' Operation            { BooleanLessThanVar $1 $3 }
             | Operation '>' Operation            { BooleanGreaterThanVar $1 $3 }
             | Operation '<=' Operation           { BooleanLessThanEqualVar $1 $3 }
             | Operation '>=' Operation           { BooleanGreaterThanEqualVar $1 $3 }
             | Operation '==' Operation           { BooleanEqualsVar $1 $3 }
             | Operation '!=' Operation           { BooleanNotEqualsVar $1 $3 }

             | Arith '<' Operation                { BooleanLessThanArithVar $1 $3 }
             | Arith '>' Operation                { BooleanGreaterThanArithVar $1 $3 }
             | Arith '<=' Operation               { BooleanLessThanEqualArithVar $1 $3 }
             | Arith '>=' Operation               { BooleanGreaterThanEqualArithVar $1 $3 }
             | Arith '==' Operation               { BooleanEqualsArithVar $1 $3 }
             | Arith '!=' Operation               { BooleanNotEqualsArithVar $1 $3 }

             | Operation '<=' Arith               { BooleanLessThanEqualVarArith $1 $3 }
             | Operation '>=' Arith               { BooleanGreaterThanEqualVarArith $1 $3 }
             | Operation '<' Arith                { BooleanLessThanVarArith $1 $3 }
             | Operation '>' Arith                { BooleanGreaterThanVarArith $1 $3 }
             | Operation '==' Arith               { BooleanEqualsVarArith $1 $3 }
             | Operation '!=' Arith               { BooleanNotEqualsVarArith $1 $3 }

RangeArith : Arith '..' Arith   { RangeDefined $1 $3 }

Arith : '(' Arith ')'               { $2 }
      | int                       { Int $1 }
      | Arith '+' Arith           { Plus $1 $3 }
      | Arith '-' Arith           { Minus $1 $3 }
      | Arith '*' Arith           { Multiply $1 $3 }
      | Arith '/' Arith           { Divide $1 $3 }
      | Operation '+' Operation   { PlusVar $1 $3 }
      | Operation '-' Operation   { MinusVar $1 $3 }
      | Operation '*' Operation   { MultiplyVar $1 $3 }
      | Operation '/' Operation   { DivideVar $1 $3 }
      | Operation '+' Arith       { PlusVarArith $1 $3 }
      | Operation '-' Arith       { MinusVarArith $1 $3 }
      | Operation '*' Arith       { MultiplyVarArith $1 $3 }
      | Operation '/' Arith       { DivideVarArith $1 $3 }
      | Arith '+' Operation       { PlusArithVar $1 $3 }
      | Arith '-' Operation       { MinusArithVar $1 $3 }
      | Arith '*' Operation       { MultiplyArithVar $1 $3 }
      | Arith '/' Operation       { DivideArithVar $1 $3 }

Type : Int      { TypeInt }
     | Tile     { TypeTile }
     | Bool     { TypeBool }
     | Range    { TypeRange }

{
parseError :: [TileToken] -> a
parseError [] = error "Unidentified Parse Error"
parseError xs = error ("Parse error at " ++ show (tokenPosn (xs !! 0)))

data Program = ProgramSingle Type Vars
             | ProgramMultiple Assignments Type Vars
             deriving Show

data Assignments = AssignmentFinal Type String Vars
                 | AssignmentMultiple Assignments Type String Vars
                 deriving Show

data Vars = Operation Operation
          | Arith Arith
          | BooleanArith BooleanArith
          | RangeArith RangeArith
           deriving Show

data Arr = TileUnidimensional Binaries
         | TileBidimensional Binaries Arrs
         deriving Show

data Arrs = TileFinal Binaries
          | TileMultiple Binaries Arrs
          deriving Show

data Binaries = IntBinary Int
              | IntComma Int Binaries
              deriving Show

data Operation = Var String
               | Tile Arr
               | HRepeatRange RangeArith Operation
               | VRepeatRange RangeArith Operation
               | HRepeat Arith Operation
               | VRepeat Arith Operation
               | HRepeatVar Operation Operation
               | VRepeatVar Operation Operation
               | HAdd Operation Operation
               | VAdd Operation Operation
               | Rot90 Operation
               | Rot180 Operation
               | Rot270 Operation
               | Grow Arith Operation
               | GrowVar Operation Operation
               | HReflect Operation
               | VReflect Operation
               | Blank Arith Arith
               | BlankArithVar Arith Operation
               | BlankVarArith Operation Arith
               | BlankVar Operation Operation
               | Not Operation
               | And Operation Operation
               | Or Operation Operation
               | ConditionThenElse BooleanArith Vars Vars
               | Subtile Vars Vars Vars Operation
               | Read String
               deriving Show

data BooleanArith = BooleanAnd BooleanArith BooleanArith
                  | BooleanOr BooleanArith BooleanArith
                  | BooleanAndVar Operation Operation
                  | BooleanOrVar Operation Operation
                  | BooleanAndVarBool Operation BooleanArith
                  | BooleanOrVarBool Operation BooleanArith
                  | BooleanAndBoolVar BooleanArith Operation
                  | BooleanOrBoolVar BooleanArith Operation

                  | BooleanLessThan Arith Arith
                  | BooleanGreaterThan Arith Arith
                  | BooleanLessThanEqual Arith Arith
                  | BooleanGreaterThanEqual Arith Arith
                  | BooleanEquals Arith Arith
                  | BooleanNotEquals Arith Arith

                  | BooleanLessThanVar Operation Operation
                  | BooleanGreaterThanVar Operation Operation
                  | BooleanLessThanEqualVar Operation Operation
                  | BooleanGreaterThanEqualVar Operation Operation
                  | BooleanEqualsVar Operation Operation
                  | BooleanNotEqualsVar Operation Operation

                  | BooleanAndBoolArithVar BooleanArith Operation
                  | BooleanOrBoolArithVar BooleanArith Operation
                  | BooleanLessThanArithVar Arith Operation
                  | BooleanGreaterThanArithVar Arith Operation
                  | BooleanLessThanEqualArithVar Arith Operation
                  | BooleanGreaterThanEqualArithVar Arith Operation
                  | BooleanEqualsArithVar Arith Operation
                  | BooleanNotEqualsArithVar Arith Operation

                  | BooleanAndVarBoolArith Operation BooleanArith
                  | BooleanOrVarBoolArith Operation BooleanArith
                  | BooleanLessThanVarArith Operation Arith
                  | BooleanGreaterThanVarArith Operation Arith
                  | BooleanLessThanEqualVarArith Operation Arith
                  | BooleanGreaterThanEqualVarArith Operation Arith
                  | BooleanEqualsVarArith Operation Arith
                  | BooleanNotEqualsVarArith Operation Arith

                  | BooleanTrue
                  | BooleanFalse
                  deriving Show

data Arith = Plus Arith Arith
           | Minus Arith Arith
           | Multiply Arith Arith
           | Divide Arith Arith
           | PlusVar Operation Operation
           | MinusVar Operation Operation
           | MultiplyVar Operation Operation
           | DivideVar Operation Operation
           | PlusVarArith Operation Arith
           | MinusVarArith Operation Arith
           | MultiplyVarArith Operation Arith
           | DivideVarArith Operation Arith
           | PlusArithVar Arith Operation
           | MinusArithVar Arith Operation
           | MultiplyArithVar Arith Operation
           | DivideArithVar Arith Operation
           | Int Int
           deriving Show

data Type = TypeInt | TypeTile | TypeBool | TypeRange deriving (Show,Eq)

data RangeArith = RangeDefined Arith Arith deriving Show
}