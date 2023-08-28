module BitGridTypeChecker where
import BitGridGrammar

type TypeEnvironment = [(String, Type)]

getVarBinding :: String -> TypeEnvironment -> Type
getVarBinding name [] = error ("No binding found for variable " ++ name)
getVarBinding name ((bindingName, bindingType) : typeEnv) | name == bindingName = bindingType
                                                          | otherwise = getVarBinding name typeEnv


addVarBinding :: String -> Type -> TypeEnvironment -> TypeEnvironment
addVarBinding name bindingType typeEnv = (name, bindingType) : typeEnv


typeOfProgram :: TypeEnvironment -> Program -> Type
typeOfProgram typeEnv (ProgramMultiple assignments finalType vars) | finalType == (typeOfVars (processAssignments [] assignments) vars) = finalType

typeOfProgram typeEnv (ProgramSingle finalType vars) | finalType == (typeOfVars typeEnv vars) = finalType
                                                     | otherwise = error ("Couldn't match type " ++ showType finalType ++ " with "
                                                                   ++ showType (typeOfVars typeEnv vars) ++ "\n" ++ "In final assignment")

typeOfProgram typeEnv _ = error ("Couldn't match type for final assignment")


processAssignments :: TypeEnvironment -> Assignments -> TypeEnvironment
processAssignments typeEnv (AssignmentMultiple nextAssignments varType varName vars) | varType == varsType = addVarBinding varName varsType newTypeEnv
                                                                                     where newTypeEnv = processAssignments typeEnv nextAssignments
                                                                                           varsType = typeOfVars newTypeEnv vars

processAssignments typeEnv (AssignmentFinal varType varName vars) | varType == (typeOfVars typeEnv vars) = addVarBinding varName (typeOfVars typeEnv vars) typeEnv
                                                                  | otherwise = error ("Couldn't match type " ++ showType varType ++ " with "
                                                                                ++ showType (typeOfVars typeEnv vars) ++ "\n" ++ "In " ++ varName ++ " assignment")

processAssignments typeEnv _ = error ("Couldn't match type at assignment")


typeOfVars :: TypeEnvironment -> Vars -> Type
typeOfVars typeEnv (Operation operation) = typeOfOperation typeEnv operation

typeOfVars typeEnv (Arith arith) | TypeInt == (typeOfArith typeEnv arith) = TypeInt
                                 | otherwise = error ("Couldn't match type Int with " ++ showType (typeOfArith typeEnv arith))

typeOfVars typeEnv (RangeArith range) | TypeRange == (typeOfRange typeEnv range) = TypeInt
                                 | otherwise = error ("Couldn't match type Range with " ++ showType (typeOfRange typeEnv range))

typeOfVars typeEnv (BooleanArith boolean) | TypeBool == (typeOfBoolArith typeEnv boolean) = TypeBool
                                          | otherwise = error ("Couldn't match type Bool with " ++ showType (typeOfBoolArith typeEnv boolean))


typeOfOperation :: TypeEnvironment -> Operation -> Type
typeOfOperation typeEnv (Read str) = TypeTile

typeOfOperation typeEnv (Var name) = getVarBinding name typeEnv

typeOfOperation typeEnv (Tile tile) | checkValidTile tile = TypeTile
                                    | otherwise = error ("Invalid tile construct")

typeOfOperation typeEnv (VAdd op1 op2) | (TypeTile, TypeTile) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeTile
                                       | otherwise = error ("Couldn't match type (Tile, Tile) for vAdd with (" ++ showType (typeOfOperation typeEnv op1)
                                                     ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfOperation typeEnv (HAdd op1 op2) | (TypeTile, TypeTile) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeTile
                                       | otherwise = error ("Couldn't match type (Tile, Tile) for hAdd with (" ++ showType (typeOfOperation typeEnv op1)
                                                     ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfOperation typeEnv (VRepeat rangeArith op) | (TypeInt, TypeTile) == (typeOfArith typeEnv rangeArith, typeOfOperation typeEnv op) = TypeTile
                                                | otherwise = error ("Couldn't match type (Int, Tile) for vRepeat with (" ++ showType (typeOfArith typeEnv rangeArith)
                                                              ++ ", " ++ showType (typeOfOperation typeEnv op) ++ ")")

typeOfOperation typeEnv (HRepeat rangeArith op) | (TypeInt, TypeTile) == (typeOfArith typeEnv rangeArith, typeOfOperation typeEnv op) = TypeTile
                                                | otherwise = error ("Couldn't match type (Int, Tile) for hRepeat with (" ++ showType (typeOfArith typeEnv rangeArith)
                                                              ++ ", " ++ showType (typeOfOperation typeEnv op) ++ ")")

typeOfOperation typeEnv (VRepeatVar op1 op2) | (TypeInt, TypeTile) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) ||
                                               (TypeRange, TypeTile) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeTile
                                             | otherwise = error ("Couldn't match type (Int/Range, Tile) for vRepeat with (" ++ showType (typeOfOperation typeEnv op1)
                                                           ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfOperation typeEnv (HRepeatVar op1 op2) | (TypeInt, TypeTile) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) ||
                                               (TypeRange, TypeTile) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeTile
                                             | otherwise = error ("Couldn't match type (Int/Range, Tile) for hRepeat with (" ++ showType (typeOfOperation typeEnv op1)
                                                           ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfOperation typeEnv (VRepeatRange arith op) | (TypeRange, TypeTile) == (typeOfRange typeEnv arith, typeOfOperation typeEnv op) = TypeTile
                                                | otherwise = error ("Couldn't match type (Range, Tile) for vRepeat with (" ++ showType (typeOfRange typeEnv arith)
                                                              ++ ", " ++ showType (typeOfOperation typeEnv op) ++ ")")

typeOfOperation typeEnv (HRepeatRange arith op) | (TypeRange, TypeTile) == (typeOfRange typeEnv arith, typeOfOperation typeEnv op) = TypeTile
                                                | otherwise = error ("Couldn't match type (Range, Tile) for hRepeat with (" ++ showType (typeOfRange typeEnv arith)
                                                              ++ ", " ++ showType (typeOfOperation typeEnv op) ++ ")")

typeOfOperation typeEnv (Rot90 op) | TypeTile == (typeOfOperation typeEnv op) = TypeTile
                                   | otherwise = error ("Couldn't match type Tile for rot90 with " ++ showType (typeOfOperation typeEnv op))

typeOfOperation typeEnv (Rot180 op) | TypeTile == (typeOfOperation typeEnv op) = TypeTile
                                   | otherwise = error ("Couldn't match type Tile for rot180 with " ++ showType (typeOfOperation typeEnv op))

typeOfOperation typeEnv (Rot270 op) | TypeTile == (typeOfOperation typeEnv op) = TypeTile
                                    | otherwise = error ("Couldn't match type Tile for rot270 with " ++ showType (typeOfOperation typeEnv op))

typeOfOperation typeEnv (Grow arith op) | (TypeInt, TypeTile) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeTile
                                        | otherwise = error ("Couldn't match type (Int, Tile) for grow with (" ++ showType (typeOfArith typeEnv arith)
                                                      ++ ", " ++ showType (typeOfOperation typeEnv op) ++ ")")

typeOfOperation typeEnv (GrowVar op1 op2) | (TypeInt, TypeTile) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeTile
                                          | otherwise = error ("Couldn't match type (Int, Tile) for grow with (" ++ showType (typeOfOperation typeEnv op1)
                                                        ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfOperation typeEnv (HReflect op) | TypeTile == (typeOfOperation typeEnv op) = TypeTile
                                      | otherwise = error ("Couldn't match type Tile for hReflect with " ++ showType (typeOfOperation typeEnv op))

typeOfOperation typeEnv (VReflect op) | TypeTile == (typeOfOperation typeEnv op) = TypeTile
                                      | otherwise = error ("Couldn't match type Tile for Rot90 with " ++ showType (typeOfOperation typeEnv op))

typeOfOperation typeEnv (Blank arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeTile
                                              | otherwise = error ("Couldn't match type (Int, Int) for blank with (" ++ showType (typeOfArith typeEnv arith1)
                                                            ++ ", " ++ showType (typeOfArith typeEnv arith2) ++ ")")

typeOfOperation typeEnv (BlankArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeTile
                                                 | otherwise = error ("Couldn't match type (Int, Int) for blank with (" ++ showType (typeOfArith typeEnv arith)
                                                               ++ ", " ++ showType (typeOfOperation typeEnv op) ++ ")")

typeOfOperation typeEnv (BlankVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeTile
                                                 | otherwise = error ("Couldn't match type (Int, Int) for blank with (" ++ showType (typeOfOperation typeEnv op)
                                                               ++ ", " ++ showType (typeOfArith typeEnv arith) ++ ")")

typeOfOperation typeEnv (BlankVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeTile
                                           | otherwise = error ("Couldn't match type (Int, Int) for blank with (" ++ showType (typeOfOperation typeEnv op1)
                                                         ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfOperation typeEnv (And op1 op2) | (TypeTile, TypeTile) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeTile
                                      | otherwise = error ("Couldn't match type (Tile, Tile) for and with (" ++ showType (typeOfOperation typeEnv op1)
                                                    ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfOperation typeEnv (Or op1 op2) | (TypeTile, TypeTile) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeTile
                                     | otherwise = error ("Couldn't match type (Tile, Tile) for or with (" ++ showType (typeOfOperation typeEnv op1)
                                                   ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfOperation typeEnv (Not op) | TypeTile == (typeOfOperation typeEnv op) = TypeTile
                                 | otherwise = error ("Couldn't match type Tile for not with " ++ showType (typeOfOperation typeEnv op))

typeOfOperation typeEnv (ConditionThenElse boolArith vars1 vars2) | TypeBool == typeOfBoolArith typeEnv boolArith && typeOfVars typeEnv vars1 == typeOfVars typeEnv vars2 = typeOfVars typeEnv vars2
                                                                  | otherwise = error ("Couldn't match type (Bool, Type, Type) for if-then-else with (" ++ showType (typeOfBoolArith typeEnv boolArith)
                                                                               ++ ", " ++ showType (typeOfVars typeEnv vars1) ++ ", " ++ showType (typeOfVars typeEnv vars2) ++ ")")

typeOfOperation typeEnv (Subtile var1 var2 var3 op) | (TypeInt, TypeInt, TypeInt, TypeTile) == (typeOfVars typeEnv var1, typeOfVars typeEnv var2, typeOfVars typeEnv var3, typeOfOperation typeEnv op) = TypeTile
                                                    | otherwise = error ("Couldn't match type (Int, Int, Int, Tile) for subtile (" ++ showType (typeOfVars typeEnv var1)
                                                                  ++ ", " ++ showType (typeOfVars typeEnv var2) ++ ", " ++ showType (typeOfVars typeEnv var3) ++ ", " ++  showType (typeOfOperation typeEnv op) ++ ")")

typeOfOperation _ _ = error ("Incompatible type with Tile")

typeOfArith :: TypeEnvironment -> Arith -> Type
typeOfArith typeEnv (Int int) = TypeInt

typeOfArith typeEnv (Plus arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeInt
                                         | otherwise = error ("Couldn't match type (Int, Int) for + with (" ++ showType (typeOfArith typeEnv arith1)
                                                       ++ ", " ++ showType (typeOfArith typeEnv arith2) ++ ")")

typeOfArith typeEnv (Minus arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeInt
                                          | otherwise = error ("Couldn't match type (Int, Int) for - with (" ++ showType (typeOfArith typeEnv arith1)
                                                       ++ ", " ++ showType (typeOfArith typeEnv arith2) ++ ")")

typeOfArith typeEnv (Multiply arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeInt
                                             | otherwise = error ("Couldn't match type (Int, Int) for * with (" ++ showType (typeOfArith typeEnv arith1)
                                                         ++ ", " ++ showType (typeOfArith typeEnv arith2) ++ ")")

typeOfArith typeEnv (Divide arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeInt
                                           | otherwise = error ("Couldn't match type (Int, Int) for / with (" ++ showType (typeOfArith typeEnv arith1)
                                                         ++ ", " ++ showType (typeOfArith typeEnv arith2) ++ ")")

typeOfArith typeEnv (PlusVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeInt
                                      | otherwise = error ("Couldn't match type (Int, Int) for + with (" ++ showType (typeOfOperation typeEnv op1)
                                                    ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfArith typeEnv (MinusVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeInt
                                       | otherwise = error ("Couldn't match type (Int, Int) for - with (" ++ showType (typeOfOperation typeEnv op1)
                                                     ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfArith typeEnv (MultiplyVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeInt
                                          | otherwise = error ("Couldn't match type (Int, Int) for or with (" ++ showType (typeOfOperation typeEnv op1)
                                                        ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfArith typeEnv (DivideVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeInt
                                        | otherwise = error ("Couldn't match type (Int, Int) for or with (" ++ showType (typeOfOperation typeEnv op1)
                                                      ++ ", " ++ showType (typeOfOperation typeEnv op2) ++ ")")

typeOfArith typeEnv (PlusVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeInt
                                            | otherwise = error ("Couldn't match type (Int, Int) for + with (" ++ showType (typeOfOperation typeEnv op)
                                                          ++ ", " ++ showType (typeOfArith typeEnv arith) ++ ")")

typeOfArith typeEnv (MinusVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeInt
                                             | otherwise = error ("Couldn't match type (Int, Int) for - with (" ++ showType (typeOfOperation typeEnv op)
                                                           ++ ", " ++ showType (typeOfArith typeEnv arith) ++ ")")

typeOfArith typeEnv (MultiplyVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeInt
                                                | otherwise = error ("Couldn't match type (Int, Int) for or with (" ++ showType (typeOfOperation typeEnv op)
                                                              ++ ", " ++ showType (typeOfArith typeEnv arith) ++ ")")

typeOfArith typeEnv (DivideVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeInt
                                              | otherwise = error ("Couldn't match type (Int, Int) for or with (" ++ showType (typeOfOperation typeEnv op)
                                                            ++ ", " ++ showType (typeOfArith typeEnv arith) ++ ")")

typeOfArith typeEnv (PlusArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeInt
                                            | otherwise = error ("Couldn't match type (Int, Int) for + with (" ++ showType (typeOfArith typeEnv arith)
                                                          ++ ", " ++ showType (typeOfOperation typeEnv op) ++ ")")

typeOfArith typeEnv (MinusArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeInt
                                             | otherwise = error ("Couldn't match type (Int, Int) for - with (" ++ showType (typeOfArith typeEnv arith)
                                                                               ++ ", " ++ showType (typeOfOperation typeEnv op) ++ ")")

typeOfArith typeEnv (MultiplyArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeInt
                                                | otherwise = error ("Couldn't match type (Int, Int) for or with (" ++ showType (typeOfArith typeEnv arith)
                                                              ++ ", " ++ showType (typeOfOperation typeEnv op) ++ ")")

typeOfArith typeEnv (DivideArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeInt
                                              | otherwise = error ("Couldn't match type (Int, Int) for or with (" ++ showType (typeOfArith typeEnv arith)
                                                            ++ ", " ++ showType (typeOfOperation typeEnv op) ++ ")")

typeOfArith typeEnv _ = error ("Incompatible type with Int")

typeOfBoolArith :: TypeEnvironment -> BooleanArith -> Type
typeOfBoolArith typeEnv (BooleanTrue) = TypeBool
typeOfBoolArith typeEnv (BooleanFalse) = TypeBool

typeOfBoolArith typeEnv (BooleanAnd bool1 bool2) | (TypeBool, TypeBool) == (typeOfBoolArith typeEnv bool1, typeOfBoolArith typeEnv bool2) = TypeBool
                                                 | otherwise = error ("Couldn't match type (Bool, Bool) for &&")

typeOfBoolArith typeEnv (BooleanOr bool1 bool2) | (TypeBool, TypeBool) == (typeOfBoolArith typeEnv bool1, typeOfBoolArith typeEnv bool2) = TypeBool
                                                | otherwise = error ("Couldn't match type (Bool, Bool) for ||")

typeOfBoolArith typeEnv (BooleanAndVar op1 op2) | (TypeBool, TypeBool) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeBool
                                                | otherwise = error ("Couldn't match type (Bool, Bool) for &&")

typeOfBoolArith typeEnv (BooleanOrVar op1 op2) | (TypeBool, TypeBool) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeBool
                                               | otherwise = error ("Couldn't match type (Bool, Bool) for ||")

typeOfBoolArith typeEnv (BooleanAndVarBool op bool) | (TypeBool, TypeBool) == (typeOfOperation typeEnv op, typeOfBoolArith typeEnv bool) = TypeBool
                                                    | otherwise = error ("Couldn't match type (Bool, Bool) for &&")

typeOfBoolArith typeEnv (BooleanOrVarBool op bool) | (TypeBool, TypeBool) == (typeOfOperation typeEnv op, typeOfBoolArith typeEnv bool) = TypeBool
                                                   | otherwise = error ("Couldn't match type (Bool, Bool) for ||")

typeOfBoolArith typeEnv (BooleanAndBoolVar bool op) | (TypeBool, TypeBool) == (typeOfBoolArith typeEnv bool, typeOfOperation typeEnv op) = TypeBool
                                                    | otherwise = error ("Couldn't match type (Bool, Bool) for &&")

typeOfBoolArith typeEnv (BooleanOrBoolVar bool op) | (TypeBool, TypeBool) == (typeOfBoolArith typeEnv bool, typeOfOperation typeEnv op) = TypeBool
                                                   | otherwise = error ("Couldn't match type (Bool, Bool) for ||")


typeOfBoolArith typeEnv (BooleanLessThanEqualVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeBool
                                                          | otherwise = error ("Couldn't match type (Int, Int) for <=")

typeOfBoolArith typeEnv (BooleanLessThanVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeBool
                                                     | otherwise = error ("Couldn't match type (Int, Int) for <")

typeOfBoolArith typeEnv (BooleanGreaterThanEqualVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeBool
                                                             | otherwise = error ("Couldn't match type (Int, Int) for >=")

typeOfBoolArith typeEnv (BooleanGreaterThanVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeBool
                                                        | otherwise = error ("Couldn't match type (Int, Int) for >")

typeOfBoolArith typeEnv (BooleanEqualsVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeBool
                                                   | otherwise = error ("Couldn't match type (Int, Int) for ==")

typeOfBoolArith typeEnv (BooleanNotEqualsVar op1 op2) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op1, typeOfOperation typeEnv op2) = TypeBool
                                                      | otherwise = error ("Couldn't match type (Int, Int) for !=")


typeOfBoolArith typeEnv (BooleanLessThanEqualVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeBool
                                                                | otherwise = error ("Couldn't match type (Int, Int) for <=")

typeOfBoolArith typeEnv (BooleanLessThanVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeBool
                                                           | otherwise = error ("Couldn't match type (Int, Int) for <")

typeOfBoolArith typeEnv (BooleanGreaterThanEqualVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeBool
                                                                   | otherwise = error ("Couldn't match type (Int, Int) for >=")

typeOfBoolArith typeEnv (BooleanGreaterThanVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeBool
                                                              | otherwise = error ("Couldn't match type (Int, Int) for >")

typeOfBoolArith typeEnv (BooleanEqualsVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeBool
                                                         | otherwise = error ("Couldn't match type (Int, Int) for ==")

typeOfBoolArith typeEnv (BooleanNotEqualsVarArith op arith) | (TypeInt, TypeInt) == (typeOfOperation typeEnv op, typeOfArith typeEnv arith) = TypeBool
                                                            | otherwise = error ("Couldn't match type (Int, Int) for !=")


typeOfBoolArith typeEnv (BooleanLessThanEqualArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeBool
                                                                | otherwise = error ("Couldn't match type (Int, Int) for <=")

typeOfBoolArith typeEnv (BooleanLessThanArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeBool
                                                           | otherwise = error ("Couldn't match type (Int, Int) for <")

typeOfBoolArith typeEnv (BooleanGreaterThanEqualArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeBool
                                                                   | otherwise = error ("Couldn't match type (Int, Int) for >=")

typeOfBoolArith typeEnv (BooleanGreaterThanArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeBool
                                                              | otherwise = error ("Couldn't match type (Int, Int) for >")

typeOfBoolArith typeEnv (BooleanEqualsArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeBool
                                                         | otherwise = error ("Couldn't match type (Int, Int) for ==")

typeOfBoolArith typeEnv (BooleanNotEqualsArithVar arith op) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith, typeOfOperation typeEnv op) = TypeBool
                                                            | otherwise = error ("Couldn't match type (Int, Int) for !=")



typeOfBoolArith typeEnv (BooleanLessThanEqual arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeBool
                                                             | otherwise = error ("Couldn't match type (Int, Int) for <=")

typeOfBoolArith typeEnv (BooleanLessThan arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeBool
                                                        | otherwise = error ("Couldn't match type (Int, Int) for <")

typeOfBoolArith typeEnv (BooleanGreaterThanEqual arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeBool
                                                                | otherwise = error ("Couldn't match type (Int, Int) for >=")

typeOfBoolArith typeEnv (BooleanGreaterThan arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeBool
                                                           | otherwise = error ("Couldn't match type (Int, Int) for >")

typeOfBoolArith typeEnv (BooleanEquals arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeBool
                                                      | otherwise = error ("Couldn't match type (Int, Int) for ==")

typeOfBoolArith typeEnv (BooleanNotEquals arith1 arith2) | (TypeInt, TypeInt) == (typeOfArith typeEnv arith1, typeOfArith typeEnv arith2) = TypeBool
                                                         | otherwise = error ("Couldn't match type (Int, Int) for !=")

typeOfBoolArith typeEnv _ = error ("Incompatible type with Bool")

typeOfRange :: TypeEnvironment -> RangeArith -> Type
typeOfRange typeEnv (RangeDefined start end) | (TypeInt, TypeInt) == (typeOfArith typeEnv start, typeOfArith typeEnv end) = TypeRange
                                             | otherwise = error ("Couldn't match type Int..Int for Range with " ++ showType (typeOfArith typeEnv start)
                                                           ++ ".." ++ showType (typeOfArith typeEnv end) )

typeOfRange typeEnv _ = error ("Incompatible type with Range")

checkValidTile :: Arr -> Bool
checkValidTile (TileUnidimensional row) = checkValidCells row
checkValidTile (TileBidimensional row rows) = checkValidCells row && checkValidTiles rows && countTileCells row == countTilesCells rows

checkValidTiles :: Arrs -> Bool
checkValidTiles (TileFinal row) = checkValidCells row
checkValidTiles (TileMultiple row rows) = checkValidCells row && checkValidTiles rows

countTilesCells :: Arrs -> Int
countTilesCells (TileFinal row) = countTileCells row
countTilesCells (TileMultiple row rows) | countTileCells row == countTilesCells rows = countTileCells row
                                        | otherwise = 0

countTileCells :: Binaries -> Int
countTileCells (IntBinary cell) = 1
countTileCells (IntComma cell cells) = 1 + countTileCells cells

checkValidCells :: Binaries -> Bool
checkValidCells (IntBinary cell) | cell == 0 || cell == 1 = True
                                 | otherwise = False

checkValidCells (IntComma cell binary) | cell == 0 || cell == 1 = checkValidCells binary
                                       | otherwise = False

showType :: Type -> String
showType TypeInt = "Integer"
showType TypeTile = "Tile"
showType TypeBool = "Boolean"
showType TypeRange = "Range"