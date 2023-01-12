import Challenges

main = do
    print $ "<------ Running testing suite ------>"
    print $ ""
    print $ "|-----------------------------------|"
    print $ "|------- Running Challenge1 --------|"
    print $ "|-----------------------------------|"
    print $ "Absorb basic: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8)] [(East,6)] == [((East,6),Absorb)])
    print $ "Absorb at edge: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8)] [(South,7)] == [((South,7),Absorb)])
    print $ "Absorb at edge pair: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8) , (6,8)] [(South,7)] == [((South,7),Absorb)])
    print $ "Absorb at edge three: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8) , (6,8) , (8,8)] [(South,7)] == [((South,7),Absorb)])
    print $ "Absorb at pair: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8) , (4,5)] [(East,6)] == [((East,6),Absorb)])
    print $ "Absorb at three: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8) , (4,5) , (4,7)] [(East,6)] == [((East,6),Absorb)])
    print $ "<----------------------------------->"
    print $ "Reflect basic: " ++ show (calcBBInteractions 8 [(2,3) , (4,3) , (7,3) , (4,6) , (7,8)] [(North,3)] == [((North,3),Reflect)])
    print $ "Reflect at edge: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8)] [(South,6)] == [((South,6),Reflect)])
    print $ "Reflect at edge pair: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8), (8,8)] [(South,6)] == [((South,6),Reflect)])
    print $ "Move and reflect: " ++ show (calcBBInteractions 8 [(3,5) , (8,5) , (8,7)] [(South,4)] == [((South,4),Reflect)])
    print $ "<----------------------------------->"
    print $ "Basic 1: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8)] [(North,1)] == [((North,1),Path (West,2))])
    print $ "Basic 2: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8)] [(East,7)] == [((East,7),Path (East,4))])
    print $ "<----------------------------------->"
    print $ "Empty input: " ++ show (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8)] [] == [] )
    print $ "Final specification example: " ++ show (listEquals (calcBBInteractions 8 [(2,3) , (7,3) , (4,6) , (7,8)] [(North,1) , (North,5) , (East, 7) , (South, 7) , (West,3) , (West,1)])
         [((North,1),Path (West,2)), ((North,5),Path (East,5)), ((East,7),Path (East,4)), ((South,7),Absorb), ((West,1),Path (East,1)), ((West,3),Absorb)] )
    print $ "Final 5x5: " ++ show (listEquals (calcBBInteractions 5 [(1,4) , (2,3) , (4,3)]
             [(North,1) , (North,2) , (North,3) , (North,4) , (North,5) , (South,1) , (South,2) , (South,3) , (South,4) , (South,5), (East,1) , (East,2) , (East,3) , (East,4) , (East,5) , (West,1) , (West,2) , (West,3) , (West,4) , (West,5)])
             [((North,1),Path (West,2)), ((North,2),Absorb) , ((North,3),Reflect), ((North,4),Absorb) , ((North,5),Path (East,2)) , ((South,1),Absorb), ((South,2),Path (East,5)), ((South,3),Reflect) , ((South, 4),Absorb), ((South,5),Path (East,4)), ((East,1),Path (West,1)), ((East,2),Path (North,5)), ((East,3),Absorb), ((East,4),Path (South,5)), ((East,5),Path (South,2)), ((West,1),Path (East,1)) , ((West,2), Path (North,1)), ((West,3),Reflect), ((West,4),Absorb), ((West,5),Reflect)] )
    print $ ""
    print $ "|-----------------------------------|"
    print $ "|------- Running Challenge2 --------|"
    print $ "|-----------------------------------|"
    print $ "Basic 1 atom 1 absorb: " ++ show (listEquals (solveBB 8 [((North,2),Absorb)]) [(2,1)])
    print $ "Basic 1 atom 2 absorb: " ++ show (listEquals (solveBB 8 [((North,2),Absorb),((South,2),Absorb)]) [(2,1)])
    print $ "Basic 1 atom 3 absorb: " ++ show (listEquals (solveBB 8 [((North,2),Absorb),((South,2),Absorb),((West,5),Absorb)]) [(2,5)])
    print $ "Basic 2 atom 2 absorb: " ++ show (listEquals (solveBB 8 [((North,2),Absorb),((South,5),Absorb)]) [(1,2),(5,1)])
    print $ "<----------------------------------->"
    print $ "Empty input: " ++ show (listEquals (solveBB 8 []) [])
    print $ "Empty solution: " ++ show (listEquals (solveBB 8 [((North,2),Path (South,2))]) [])
    print $ "Final specification example: " ++ show (listEquals
        (solveBB 8 [((North,1),Path (West,2)), ((North,5),Path (East,5)), ((East,7),Path (East,4)), ((South,7),Absorb), ((West,1),Path (East,1)), ((West,3),Absorb)]) [(2,3) ,(7,3) , (4,6) , (7,8)])
    print $ "Final 5x5: " ++ show (listEquals (solveBB 5
        (calcBBInteractions 5 [(1,4) , (2,3) , (4,3)] [(North,1) , (North,2) , (North,3) , (North,4) , (North,5) , (South,1) , (South,2) , (South,3) , (South,4) , (South,5), (East,1) , (East,2) , (East,3) , (East,4) , (East,5) , (West,1) , (West,2) , (West,3) , (West,4) , (West,5)])) [(1,4) , (2,3) , (4,3)])
    print $ ""
    print $ "|-----------------------------------|"
    print $ "|------- Running Challenge3 --------|"
    print $ "|-----------------------------------|"
    print $ "toANF specification example1: " ++ show (toANF (LamApp (LamVar 1) (LamVar 0)) 0 == LamApp (LamVar 1) (LamVar 0))
    print $ "toANF specification example2: " ++ show (toANF (LamAbs 3 (LamVar 2)) 0 == LamAbs 0 (LamVar 2))
    print $ "toANF specification example3: " ++ show (toANF (LamAbs 0 (LamAbs 1 (LamVar 0))) 0 == LamAbs 0 (LamAbs 1 (LamVar 0)))
    print $ "toANF specification example4: " ++ show (toANF (LamAbs 1 (LamAbs 0 (LamVar 1))) 0 == LamAbs 0 (LamAbs 1 (LamVar 0)))
    print $ "toANF specification example5: " ++ show (toANF (LamAbs 1 (LamAbs 0 (LamVar 0))) 0 == LamAbs 0 (LamAbs 0 (LamVar 0)))
    print $ "toANF specification example6: " ++ show (toANF (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 0)))) 0 == LamAbs 0 (LamAbs 1 (LamAbs 1 (LamVar 0))))
    print $ "<----------------------------------->"
    print $ "toANF specification extra1: " ++ show (toANF (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))) 0 == LamApp (LamAbs 0 (LamVar 0)) (LamAbs 0 (LamVar 0)))
    print $ "toANF specification extra2: " ++ show (toANF (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1)))) 0  == LamAbs 0 (LamApp (LamVar 0) (LamAbs 0 (LamVar 0))))
    print $ "toANF specification extra3: " ++ show (toANF (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1)))) 0 == LamApp (LamVar 2) (LamAbs 0 (LamAbs 1 (LamVar 0))))
    print $ "toANF specification extra4: " ++ show (toANF (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1)))))) 0 == LamAbs 0 (LamAbs 0 (LamApp (LamVar 0) (LamAbs 0 (LamAbs 1 (LamVar 0))))))
    print $ "<----------------------------------->"
    print $ "Final specification example1: " ++ show (prettyPrint (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))) == "(\\x0 -> x0) \\x0 -> x0")
    print $ "Final specification example2: " ++ show (prettyPrint (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1)))) == "\\x0 -> x0 \\x0 -> x0")
    print $ "Final specification example3: " ++ show (prettyPrint (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1)))) == "x2 \\x0 -> \\x1 -> x0")
    print $ "Final specification example4: " ++ show (prettyPrint (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1)))))) == "\\x0 -> \\x0 -> x0 \\x0 -> \\x1 -> x0")
    print $ ""
    print $ "|-----------------------------------|"
    print $ "|------- Running Challenge4 --------|"
    print $ "|-----------------------------------|"
    print $ "Parse digit basic: " ++ show (parseArith "1" == Just (ArithNum 1))
    print $ "Parse number basic: " ++ show (parseArith "123" == Just (ArithNum 123))
    print $ "Parse section basic: " ++ show (parseArith "(+1)2" == Just (SecApp (Section (ArithNum 1)) (ArithNum 2)))
    print $ "Parse bracket basic: " ++ show (parseArith "(1)" == Just (ArithNum 1))
    print $ "Parse addition basic: " ++ show (parseArith "1 + 2" == Just (Add (ArithNum 1) (ArithNum 2)))
    print $ "Parse mult basic: " ++ show (parseArith "1 * 24" == Just (Mul (ArithNum 1) (ArithNum 24)))
    print $ "<----------------------------------->"
    print $ "Fail1: " ++ show (parseArith "+ 1 + 2" == Nothing)
    print $ "Fail2: " ++ show (parseArith "(+1) (+1)" == Nothing)
    print $ "Fail3: " ++ show (parseArith "555 +" == Nothing)
    print $ "Fail4: " ++ show (parseArith "-764" == Nothing)
    print $ "Fail5: " ++ show (parseArith "  " == Nothing)
    print $ "Fail6: " ++ show (parseArith "(+1) " == Nothing)
    print $ "Fail7: " ++ show (parseArith "((+1)" == Nothing)
    print $ "Fail8: " ++ show (parseArith "333) " == Nothing)
    print $ "Fail9: " ++ show (parseArith "(2) (3) " == Nothing)
    print $ "Fail10: " ++ show (parseArith "1 2" == Nothing)
    print $ "<----------------------------------->"
    print $ "Empty input: " ++ show (parseArith "" == Nothing)
    print $ "Final specification example1: " ++ show (parseArith "1 + 2" == Just (Add (ArithNum 1) (ArithNum 2)))
    print $ "Final specification example2: " ++ show (parseArith "(+1) 2" == Just (SecApp (Section (ArithNum 1)) (ArithNum 2)))
    print $ "Final specification example3: " ++ show (parseArith "2 (+1)" == Nothing)
    print $ "Final specification example4: " ++ show (parseArith "(+1) (+2) 3" == Just (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 2)) (ArithNum 3))))
    print $ "Final specification example5: " ++ show (parseArith " ( + ( 5 * 2 ) ) 6 + 7 " == Just (Add ( SecApp ( Section ( Mul ( ArithNum 5 ) ( ArithNum 2 ))) ( ArithNum 6 )) ( ArithNum 7 )))
    print $ ""
    print $ "|-----------------------------------|"
    print $ "|------- Running Challenge5 --------|"
    print $ "|-----------------------------------|"
    print $ "Num 0 basic: " ++ show (churchEnc (ArithNum 0) == (LamAbs 0 (LamAbs 1 (LamVar 1))))
    print $ "Num 1 basic: " ++ show (churchEnc (ArithNum 1) == (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1)))))
    print $ "Num 3 basic: " ++ show (churchEnc (ArithNum 3) == (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))
    print $ "<----------------------------------->"
    print $ "Add basic: " ++ show (churchEnc (Add (ArithNum 1) (ArithNum 1)) == (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))))
    print $ "Sec basic: " ++ show (churchEnc (SecApp (Section (ArithNum 1)) (ArithNum 1)) == (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))))
    print $ "Mul basic: " ++ show (churchEnc (Mul (ArithNum 4) (ArithNum 5)) ==
        (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))))))
    print $ "Add + Sec: " ++ show (churchEnc (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 1)) (Add (ArithNum 1) (ArithNum 1)))) ==
        (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1)))))
        (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))))))
    print $ ""
    print $ "|-----------------------------------|"
    print $ "|------- Running Challenge6 --------|"
    print $ "|-----------------------------------|"
    print $ "Add simple: " ++ show (compareArithLam (Add (ArithNum 1) (ArithNum 1)) == (1,6))
    print $ "Mul simple: " ++ show (compareArithLam (Mul (ArithNum 1) (ArithNum 1)) == (1,6))
    print $ "Sec simple: " ++ show (compareArithLam (SecApp (Section (ArithNum 1)) (ArithNum 4)) == (1,6))
    print $ "<----------------------------------->"
    print $ "Add multiple: " ++ show (compareArithLam (Add (Add (ArithNum 1) (ArithNum 2)) (Add (ArithNum 1) (ArithNum 2))) == (2,18))
    print $ "Mul multiple: " ++ show (compareArithLam (Mul (Mul (ArithNum 1) (ArithNum 2)) (Mul (ArithNum 1) (ArithNum 2))) == (2,26))
    print $ "Sec multiple: " ++ show (compareArithLam (SecApp (Section (SecApp (Section (ArithNum 1)) (ArithNum 4))) (SecApp (Section (ArithNum 1)) (ArithNum 4))) == (2,18))
    print $ "<----------------------------------->"
    print $ "Final specification example1: " ++ show (compareArithLam (ArithNum 4) == (0,0))
    print $ "Final specification example2: " ++ show (compareArithLam (Add (ArithNum 4) (ArithNum 5)) == (1,6))
    print $ "Final specification example3: " ++ show (compareArithLam (Mul (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3)) == (2,28))
    print $ "Final specification example4: " ++ show (compareArithLam (Mul (Add (SecApp (Section (ArithNum 1)) (ArithNum 4)) (ArithNum 5)) (ArithNum 3)) == (3,36))