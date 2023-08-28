module BitGridEval where
import BitGridGrammar
import Data.Array
import Data.List
import System.IO.Unsafe

data EnvironmentVar = Integer Int | Boolean Bool | ArrayUni [Int] | ArrayBi [[Int]] | Range (Int, Int, Int) deriving (Eq, Show)

type Environment = [(String, EnvironmentVar)]

-- Extract variable value from environment
getVarBindingEval :: String -> Environment -> EnvironmentVar
getVarBindingEval name [] = error ("No binding found for variable " ++ name)
getVarBindingEval name ((bindingName, value) : env) | name == bindingName = value
                                                    | otherwise = getVarBindingEval name env

-- Update the environment
updateEnvironment :: String -> EnvironmentVar -> Environment -> Environment
updateEnvironment name value [] = [(name, value)]
updateEnvironment name value ((envName, envValue):env) | name == envName = ((envName, value):env)
                                                       | otherwise = (envName, envValue) : updateEnvironment name value env

evalProgram :: Environment -> Program -> Environment
evalProgram env (ProgramSingle _ vars) = updateEnvironment "final" (evalVars env vars) env

evalProgram env (ProgramMultiple assignments _ vars) = updateEnvironment "final" (evalVars (evalProcessAssignments env assignments) vars) env

evalProcessAssignments :: Environment -> Assignments -> Environment
evalProcessAssignments env (AssignmentFinal _ varName vars) = updateEnvironment varName (evalVars env vars) env

evalProcessAssignments env (AssignmentMultiple nextAssignments _ varName vars) = updateEnvironment varName (evalVars newEnv vars) (env ++ newEnv)
                                                                                  where newEnv = evalProcessAssignments env nextAssignments

evalVars :: Environment -> Vars -> EnvironmentVar
evalVars env (Arith arith) = evalArith env arith
evalVars env (Operation op) = evalOperation env op
evalVars env (BooleanArith boolArith) = evalBooleanArith env boolArith
evalVars env (RangeArith rangeArith) = evalRangeArith env rangeArith

evalOperation :: Environment -> Operation -> EnvironmentVar
evalOperation env (Tile tile) = buildTile tile
evalOperation env (Var name) = getVarBindingEval name env
evalOperation env (Read fileName) = readTile fileName

evalOperation env (VAdd op1 op2) = vAddEval (evalOperation env op1) (evalOperation env op2)
evalOperation env (HAdd op1 op2) = hAddEval (evalOperation env op1) (evalOperation env op2)

evalOperation env (HRepeat arith op) = hRepeatEval (evalArith env arith) (evalOperation env op)
evalOperation env (VRepeat arith op) = vRepeatEval (evalArith env arith) (evalOperation env op)
evalOperation env (HRepeatRange rangeArith op) = hRepeatEval (evalRangeArith env rangeArith) (evalOperation env op)
evalOperation env (VRepeatRange rangeArith op) = vRepeatEval (evalRangeArith env rangeArith) (evalOperation env op)

evalOperation env (HRepeatVar op1 op2) = hRepeatVarEval env op1 op2
evalOperation env (VRepeatVar op1 op2) = vRepeatVarEval env op1 op2

evalOperation env (Rot90 op) = rot90Eval (evalOperation env op)
evalOperation env (Rot180 op) = rot180Eval (evalOperation env op)
evalOperation env (Rot270 op) = rot270Eval (evalOperation env op)

evalOperation env (Grow arith op) = growEval (evalArith env arith) (evalOperation env op)
evalOperation env (GrowVar var op) = growEval (evalOperation env var) (evalOperation env op)

evalOperation env (HReflect op) = hReflectEval (evalOperation env op)
evalOperation env (VReflect op) = vReflectEval (evalOperation env op)

evalOperation env (Blank arith1 arith2) = blankEval (evalArith env arith1) (evalArith env arith2)
evalOperation env (BlankArithVar arith op) = blankEval (evalArith env arith) (evalOperation env op)
evalOperation env (BlankVarArith op arith) = blankEval (evalOperation env op) (evalArith env arith)
evalOperation env (BlankVar op1 op2) = blankEval (evalOperation env op1) (evalOperation env op2)

evalOperation env (Subtile var1 var2 var3 op) = subtileEval (evalVars env var1) (evalVars env var2) (evalVars env var3) (evalOperation env op)

evalOperation env (And op1 op2) = andEval (evalOperation env op1) (evalOperation env op2)
evalOperation env (Or op1 op2) = orEval (evalOperation env op1) (evalOperation env op2)
evalOperation env (Not op) = notEval (evalOperation env op)

evalOperation env (ConditionThenElse boolArith vars1 vars2) = conditionThenElseEval (evalBooleanArith env boolArith) (evalVars env vars1) (evalVars env vars2)

evalArith :: Environment -> Arith -> EnvironmentVar
evalArith env (Int int) = (Integer int)
evalArith env (Plus arith1 arith2) = plusEval (evalArith env arith1) (evalArith env arith2)
evalArith env (PlusArithVar arith op) = plusEval (evalArith env arith) (evalOperation env op)
evalArith env (PlusVarArith op arith) = plusEval (evalOperation env op) (evalArith env arith)
evalArith env (PlusVar op1 op2) = plusEval (evalOperation env op1) (evalOperation env op2)

evalArith env (Minus arith1 arith2) = minusEval (evalArith env arith1) (evalArith env arith2)
evalArith env (MinusArithVar arith op) = minusEval (evalArith env arith) (evalOperation env op)
evalArith env (MinusVarArith op arith) = minusEval (evalOperation env op) (evalArith env arith)
evalArith env (MinusVar op1 op2) = minusEval (evalOperation env op1) (evalOperation env op2)

evalArith env (Multiply arith1 arith2) = multiplyEval (evalArith env arith1) (evalArith env arith2)
evalArith env (MultiplyArithVar arith op) = multiplyEval (evalArith env arith) (evalOperation env op)
evalArith env (MultiplyVarArith op arith) = multiplyEval (evalOperation env op) (evalArith env arith)
evalArith env (MultiplyVar op1 op2) = multiplyEval (evalOperation env op1) (evalOperation env op2)

evalArith env (Divide arith1 arith2) = divideEval (evalArith env arith1) (evalArith env arith2)
evalArith env (DivideArithVar arith op) = divideEval (evalArith env arith) (evalOperation env op)
evalArith env (DivideVarArith op arith) = divideEval (evalOperation env op) (evalArith env arith)
evalArith env (DivideVar op1 op2) = divideEval (evalOperation env op1) (evalOperation env op2)

evalBooleanArith :: Environment -> BooleanArith -> EnvironmentVar
evalBooleanArith env (BooleanTrue) = (Boolean True)
evalBooleanArith env (BooleanFalse) = (Boolean False)

evalBooleanArith env (BooleanAnd bool1 bool2) = boolAndEval (evalBooleanArith env bool1) (evalBooleanArith env bool2)
evalBooleanArith env (BooleanAndBoolVar bool op) = boolAndEval (evalBooleanArith env bool) (evalOperation env op)
evalBooleanArith env (BooleanAndVarBool op bool) = boolAndEval (evalOperation env op) (evalBooleanArith env bool)
evalBooleanArith env (BooleanAndVar op1 op2) = boolAndEval (evalOperation env op1) (evalOperation env op2)
evalBooleanArith env (BooleanOr bool1 bool2) = boolOrEval (evalBooleanArith env bool1) (evalBooleanArith env bool2)
evalBooleanArith env (BooleanOrBoolVar bool op) = boolOrEval (evalBooleanArith env bool) (evalOperation env op)
evalBooleanArith env (BooleanOrVarBool op bool) = boolOrEval (evalOperation env op) (evalBooleanArith env bool)
evalBooleanArith env (BooleanOrVar op1 op2) = boolOrEval (evalOperation env op1) (evalOperation env op2)

evalBooleanArith env (BooleanLessThan arith1 arith2) = boolLessThan (evalArith env arith1) (evalArith env arith2)
evalBooleanArith env (BooleanLessThanArithVar arith op) = boolLessThan (evalArith env arith) (evalOperation env op)
evalBooleanArith env (BooleanLessThanVarArith op arith) = boolLessThan (evalOperation env op) (evalArith env arith)
evalBooleanArith env (BooleanLessThanVar op1 op2) = boolLessThan (evalOperation env op1) (evalOperation env op2)

evalBooleanArith env (BooleanGreaterThan arith1 arith2) = boolGreaterThan (evalArith env arith1) (evalArith env arith2)
evalBooleanArith env (BooleanGreaterThanArithVar arith op) = boolGreaterThan (evalArith env arith) (evalOperation env op)
evalBooleanArith env (BooleanGreaterThanVarArith op arith) = boolGreaterThan (evalOperation env op) (evalArith env arith)
evalBooleanArith env (BooleanGreaterThanVar op1 op2) = boolGreaterThan (evalOperation env op1) (evalOperation env op2)

evalBooleanArith env (BooleanLessThanEqual arith1 arith2) = boolLessThanEqual (evalArith env arith1) (evalArith env arith2)
evalBooleanArith env (BooleanLessThanEqualArithVar arith op) = boolLessThanEqual (evalArith env arith) (evalOperation env op)
evalBooleanArith env (BooleanLessThanEqualVarArith op arith) = boolLessThanEqual (evalOperation env op) (evalArith env arith)
evalBooleanArith env (BooleanLessThanEqualVar op1 op2) = boolLessThanEqual (evalOperation env op1) (evalOperation env op2)

evalBooleanArith env (BooleanGreaterThanEqual arith1 arith2) = boolGreaterThanEqual (evalArith env arith1) (evalArith env arith2)
evalBooleanArith env (BooleanGreaterThanEqualArithVar arith op) = boolGreaterThanEqual (evalArith env arith) (evalOperation env op)
evalBooleanArith env (BooleanGreaterThanEqualVarArith op arith) = boolGreaterThanEqual (evalOperation env op) (evalArith env arith)
evalBooleanArith env (BooleanGreaterThanEqualVar op1 op2) = boolGreaterThanEqual (evalOperation env op1) (evalOperation env op2)

evalBooleanArith env (BooleanEquals arith1 arith2) = boolEquals (evalArith env arith1) (evalArith env arith2)
evalBooleanArith env (BooleanEqualsArithVar arith op) = boolEquals (evalArith env arith) (evalOperation env op)
evalBooleanArith env (BooleanEqualsVarArith op arith) = boolEquals (evalOperation env op) (evalArith env arith)
evalBooleanArith env (BooleanEqualsVar op1 op2) = boolEquals (evalOperation env op1) (evalOperation env op2)

evalBooleanArith env (BooleanNotEquals arith1 arith2) = boolNotEquals (evalArith env arith1) (evalArith env arith2)
evalBooleanArith env (BooleanNotEqualsArithVar arith op) = boolNotEquals (evalArith env arith) (evalOperation env op)
evalBooleanArith env (BooleanNotEqualsVarArith op arith) = boolNotEquals (evalOperation env op) (evalArith env arith)
evalBooleanArith env (BooleanNotEqualsVar op1 op2) = boolNotEquals (evalOperation env op1) (evalOperation env op2)


evalRangeArith :: Environment -> RangeArith -> EnvironmentVar
evalRangeArith env (RangeDefined arith1 arith2) | checkValidRange (evalArith env arith1) (evalArith env arith2)
                                                  = (Range (extractInt (evalArith env arith1), extractInt (evalArith env arith1), extractInt (evalArith env arith2)))
                                                | otherwise = error "Invalid range values, check start value is lower than end"

extractInt :: EnvironmentVar -> Int
extractInt (Integer int) = int

buildTile :: Arr -> EnvironmentVar
buildTile (TileUnidimensional cells) = (ArrayUni (buildTileUni cells))
buildTile (TileBidimensional tile tiles) = (ArrayBi (buildTileBi [(buildTileUni tile)] tiles))

buildTileUni :: Binaries -> [Int]
buildTileUni (IntBinary binary) = [binary]
buildTileUni (IntComma binary binaries) = [binary] ++ buildTileUni binaries

buildTileBi :: [[Int]] -> Arrs -> [[Int]]
buildTileBi rows (TileFinal tile) = reverse (buildTileUni tile : rows)
buildTileBi rows (TileMultiple tile tiles) = buildTileBi (buildTileUni tile : rows) tiles

vRepeatVarEval :: Environment -> Operation -> Operation -> EnvironmentVar
vRepeatVarEval env (Var varName) op = runVRepeatVarEval (updateEnvironment varName (getVarBindingEval varName env) env) varName op

runVRepeatVarEval :: Environment -> String -> Operation -> EnvironmentVar
runVRepeatVarEval env varName op | checkInt varValue = vRepeatEval (getVarBindingEval varName env)  (evalOperation env op)
                                 | otherwise = runVRepeatRangeEval env varName op
                                 where varValue = getVarBindingEval varName env

runVRepeatRangeEval :: Environment -> String -> Operation -> EnvironmentVar
runVRepeatRangeEval env varName op | checkLoop varValue = vAddEval (evalOperation (updateEnvironment varName (rangeToInt varValue) env) op) (runVRepeatVarEval (updateEnvironment varName (incrementRange varValue) env) varName op)
                                   | otherwise = evalOperation (updateEnvironment varName (rangeToInt varValue) env) op
                                   where varValue = getVarBindingEval varName env

hRepeatVarEval :: Environment -> Operation -> Operation -> EnvironmentVar
hRepeatVarEval env (Var varName) op = runHRepeatVarEval (updateEnvironment varName (getVarBindingEval varName env) env) varName op

runHRepeatVarEval :: Environment -> String -> Operation -> EnvironmentVar
runHRepeatVarEval env varName op | checkInt varValue = hRepeatEval varValue (evalOperation env op)
                                 | otherwise = runHRepeatRangeEval env varName op
                                 where varValue = getVarBindingEval varName env

runHRepeatRangeEval :: Environment -> String -> Operation -> EnvironmentVar
runHRepeatRangeEval env varName op | checkLoop varValue = hAddEval (evalOperation (updateEnvironment varName (rangeToInt varValue) env) op) (runHRepeatVarEval (updateEnvironment varName (incrementRange varValue) env) varName op)
                                   | otherwise = evalOperation (updateEnvironment varName (rangeToInt varValue) env) op
                                   where varValue = getVarBindingEval varName env

checkInt :: EnvironmentVar -> Bool
checkInt (Integer _) = True
checkInt _ = False

intToRange :: EnvironmentVar -> EnvironmentVar
intToRange (Integer int) = (Range (0,0,int))

checkLoop :: EnvironmentVar -> Bool
checkLoop (Range (value,_,stop)) = value < stop

rangeToInt :: EnvironmentVar -> EnvironmentVar
rangeToInt (Range (value,_,_)) = (Integer value)

incrementRange :: EnvironmentVar -> EnvironmentVar
incrementRange (Range (value,start,stop)) = (Range (value+1,start,stop))

boolEquals :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
boolEquals (Integer int1) (Integer int2) = (Boolean (int1 == int2))

boolNotEquals :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
boolNotEquals (Integer int1) (Integer int2) = (Boolean (int1 /= int2))

boolGreaterThanEqual :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
boolGreaterThanEqual (Integer int1) (Integer int2) = (Boolean (int1 >= int2))

boolLessThanEqual :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
boolLessThanEqual (Integer int1) (Integer int2) = (Boolean (int1 <= int2))

boolGreaterThan :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
boolGreaterThan (Integer int1) (Integer int2) = (Boolean (int1 > int2))

boolLessThan :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
boolLessThan (Integer int1) (Integer int2) = (Boolean (int1 < int2))

boolOrEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
boolOrEval (Boolean bool1) (Boolean bool2) = (Boolean (bool1 || bool2))

boolAndEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
boolAndEval (Boolean bool1) (Boolean bool2) = (Boolean (bool1 && bool2))

conditionThenElseEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar -> EnvironmentVar
conditionThenElseEval (Boolean bool) envVar1 envVar2 | bool = envVar1
                                                     | otherwise = envVar2

notEval :: EnvironmentVar -> EnvironmentVar
notEval (ArrayUni arr) = (ArrayUni (map (\x -> if x == 0 then 1 else 0) arr))
notEval (ArrayBi arr) = (ArrayBi (map (\y -> map (\x -> if x == 0 then 1 else 0) y) arr))

andEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
andEval (ArrayUni _) (ArrayBi _) = error "Can't apply AND on unidimensional tile with bidimensional tile"
andEval (ArrayBi _) (ArrayUni _) = error "Can't apply AND on unidimensional tile with bidimensional tile"
andEval (ArrayUni arr1) (ArrayUni arr2) = (ArrayUni (zipWith andBinary arr1 arr2))
andEval (ArrayBi arr1) (ArrayBi arr2) = (ArrayBi (zipWith (\x y-> zipWith andBinary x y) arr1 arr2))

andBinary :: Int -> Int -> Int
andBinary 1 1 = 1
andBinary _ _ = 0

orEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
orEval (ArrayUni _) (ArrayBi _) = error "Can't apply OR on unidimensional tile with bidimensional tile"
orEval (ArrayBi _) (ArrayUni _) = error "Can't apply OR on unidimensional tile with bidimensional tile"
orEval (ArrayUni arr1) (ArrayUni arr2) = (ArrayUni (zipWith andBinary arr1 arr2))
orEval (ArrayBi arr1) (ArrayBi arr2) = (ArrayBi (zipWith (\x y-> zipWith orBinary x y) arr1 arr2))

orBinary :: Int -> Int -> Int
orBinary 0 0 = 0
orBinary _ _ = 1

subtileEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar -> EnvironmentVar -> EnvironmentVar
subtileEval (Integer 0) _ _ _ = (ArrayUni [])
subtileEval (Integer size) (Integer 0) (Integer 0) (ArrayUni arr) | size <= length arr = (ArrayUni (take size $ arr))
                                                                  | otherwise = error ("Can't take subtile of size " ++ show size ++
                                                                                " from (0,0) in\n" ++ (showArrUni arr))

subtileEval (Integer size) (Integer x) (Integer 0) (ArrayUni arr) | size + x <= length arr = (ArrayUni (subtileUniEval x (x + size) arr))
                                                                  | otherwise = error ("Can't take subtile of size " ++ show size ++
                                                                                " from (" ++ show x ++ ",0) in\n" ++ (showArrUni arr))

subtileEval (Integer size) (Integer x) (Integer y) (ArrayUni arr) = error ("Can't take bidimensional subtile from unidimensional tile")

subtileEval (Integer size) (Integer x) (Integer y) (ArrayBi arr) | checkSize size x y arr = (ArrayBi (subtileBiEval x (x + size) y (y + size) arr))
                                                                 | otherwise = error ("Can't take subtile of size " ++ show size ++
                                                                                      " from (" ++ show x ++ "," ++ show y ++ ") in\n" ++ (showArrBi arr))

subtileBiEval :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
subtileBiEval startX sizeX startY sizeY arrs = map (\x -> drop startX . take sizeX $ x) (drop startY . take sizeY $ arrs)

checkSize :: Int -> Int -> Int -> [[Int]] -> Bool
checkSize size x y (arr:arrs) = size + x <= length arr && size + y <= length (arr:arrs)

subtileUniEval :: Int -> Int -> [Int] -> [Int]
subtileUniEval start size arr = drop start . take size $ arr

growEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
growEval (Integer factor) (ArrayUni arr) = (ArrayBi (replicate factor (concatMap (replicate factor) arr)))
growEval (Integer factor) (ArrayBi arr) = (ArrayBi (concatMap (replicate factor) (map (\x -> concatMap (replicate factor) x) arr)))

rot90Eval :: EnvironmentVar -> EnvironmentVar
rot90Eval (ArrayUni arr) = (ArrayBi (rot90EvalUni arr))
rot90Eval (ArrayBi arr) = (ArrayBi (transpose (reverse arr)))

rot180Eval :: EnvironmentVar -> EnvironmentVar
rot180Eval (ArrayUni arr) = (ArrayUni (reverse arr))
rot180Eval envVar = rot90Eval (rot90Eval envVar)

rot270Eval :: EnvironmentVar -> EnvironmentVar
rot270Eval (ArrayUni arr) = (ArrayBi (rot90EvalUni (reverse arr)))
rot270Eval envVar = rot90Eval (rot90Eval (rot90Eval envVar))

rot90EvalUni :: [Int] -> [[Int]]
rot90EvalUni [x] = [[x]]
rot90EvalUni (x:xs) = [[x]] ++ rot90EvalUni xs

blankEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
blankEval (Integer 0) _ = (ArrayUni [])
blankEval _ (Integer 0) = (ArrayUni [])
blankEval (Integer length) (Integer 1) = (ArrayUni (replicate length 0))
blankEval (Integer length) (Integer height) = vRepeatEval (Integer height) (ArrayUni (replicate length 0))

hReflectEval:: EnvironmentVar -> EnvironmentVar
hReflectEval (ArrayUni arr) = (ArrayUni (reverse arr))
hReflectEval (ArrayBi arr) = (ArrayBi (map (\x -> reverse x) arr))

vReflectEval:: EnvironmentVar -> EnvironmentVar
vReflectEval (ArrayUni arr) = (ArrayUni arr)
vReflectEval (ArrayBi arr) = (ArrayBi (reverse arr))

vAddEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
vAddEval (ArrayUni arr1) (ArrayUni arr2) | checkValidTile (arr1 : [arr2]) = (ArrayBi (arr1 : [arr2]))
                                         | otherwise = error ("Invalid resulting tile construct in vAdd " ++ showValue (ArrayBi (arr1 : [arr2])))
vAddEval (ArrayUni arr1) (ArrayBi arr2) | checkValidTile (arr1 : arr2) = (ArrayBi (arr1 : arr2))
                                        | otherwise = error "Invalid resulting tile construct in vAdd"
vAddEval (ArrayBi arr1) (ArrayUni arr2) | checkValidTile (arr1 ++ [arr2]) = (ArrayBi (arr1 ++ [arr2]))
                                        | otherwise = error "Invalid resulting tile construct in vAdd"
vAddEval (ArrayBi arr1) (ArrayBi arr2) | checkValidTile (arr1 ++ arr2) = (ArrayBi (arr1 ++ arr2))
                                       | otherwise = error ("Invalid resulting tile construct in vAdd " ++ showValue (ArrayBi (arr1 ++ arr2)))


hAddEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
hAddEval (ArrayUni arr1) (ArrayUni arr2) = (ArrayUni (arr1 ++ arr2))
hAddEval (ArrayBi arr1) (ArrayBi arr2) | checkValidTile (combineH arr1 arr2) = (ArrayBi (combineH arr1 arr2))
                                       | otherwise = error "Invalid resulting tile construct in hAdd"

hAddEval _ _ = error "Incompatible vAdd tiles"

combineH :: [[Int]] -> [[Int]] -> [[Int]]
combineH [] [arr] = error "Incompatible hAdd tiles"
combineH [arr] [] = error "Incompatible hAdd tiles"

combineH [arr1] [arr2] = [arr1 ++ arr2]
combineH (arr1:arrs1) (arr2:arrs2) = (arr1 ++ arr2) : (combineH arrs1 arrs2)

combineH _ _ = error "Incompatible hAdd tiles"


hRepeatEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
hRepeatEval (Integer 1) envVar = envVar
hRepeatEval (Integer repetitions) envVar = hAddEval envVar (hRepeatEval (Integer (repetitions-1)) envVar)
hRepeatEval (Range (_,start,end)) envVar = hRepeatEval (Integer (end-start+1)) envVar

vRepeatEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
vRepeatEval (Integer 1) envVar = envVar
vRepeatEval (Integer repetitions) envVar = vAddEval envVar (vRepeatEval (Integer (repetitions-1)) envVar)
vRepeatEval (Range (_,start,end)) envVar = vRepeatEval (Integer (end-start+1)) envVar

minusEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
minusEval (Integer int1) (Integer int2) = (Integer (int1 - int2))

plusEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
plusEval (Integer int1) (Integer int2) = (Integer (int1 + int2))

multiplyEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
multiplyEval (Integer int1) (Integer int2) = (Integer (int1 * int2))

divideEval :: EnvironmentVar -> EnvironmentVar -> EnvironmentVar
divideEval (Integer int1) (Integer int2) = (Integer (int1 `div` int2))

checkValidTile :: [[a]] -> Bool
checkValidTile [] = True
checkValidTile (x:xs) = all (\y -> length y == length x) xs

checkValidRange :: EnvironmentVar -> EnvironmentVar -> Bool
checkValidRange (Integer start) (Integer end) | start < end = True
                                              | otherwise = False

showResult :: Environment -> String
showResult ((bindingName, value) : env) | bindingName == "final" = showValue value
                                        | otherwise = showResult env

showValue :: EnvironmentVar -> String
showValue (Integer int) = show int
showValue (ArrayUni arr) = showArrUni arr
showValue (ArrayBi arr) = showArrBi arr
showValue (Boolean bool) = show bool
showValue (Range (_,start,end)) = show start ++ ".." ++ show end

showArrUni :: [Int] -> String
showArrUni [] = ""
showArrUni [x] = show x
showArrUni (x:arr) = show x ++ showArrUni arr

showArrBi :: [[Int]] -> String
showArrBi [x] = showArrUni x
showArrBi (x:arr) = showArrUni x ++ "\n" ++ showArrBi arr

readTile :: String -> EnvironmentVar
readTile fileName | length (readAndIndex fileName) == 1 = (ArrayUni (head (readAndIndex fileName)))
                  | otherwise = (ArrayBi (readAndIndex fileName))

parseInpTile :: String -> [[Int]]
parseInpTile tile = map (map (\c -> if c == '0' then 0 else 1)) (lines tile)

readAndIndex :: FilePath -> [[Int]]
readAndIndex fileName = parseInpTile $ unsafePerformIO $ readFile (fileName)