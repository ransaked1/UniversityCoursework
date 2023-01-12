{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

--module Exercises (Expr(..),Binding,Interpretation,consistent,solve,satisfiable) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

import Data.List
import Data.Ord

-- Exercise A8
data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord, Show, Read)
type Binding = (Char, Bool)
type Interpretation = [Binding]

--Utilitarian
uniqueTokens :: Interpretation -> [Char]
uniqueTokens lst = nub [ e1 | (e1, e2) <- lst]

xnor :: Bool -> Bool -> Bool
xnor True True = True
xnor True False = False
xnor False True = False
xnor False False = True

xnorFunc :: [Bool] -> Bool
xnorFunc (x:[]) = True
xnorFunc (x1:x2:xs) | xnor x1 x2 = xnorFunc ([x2] ++ xs)
                    | otherwise = False

consistentCrossProduct :: [Interpretation] -> [Interpretation] -> [Interpretation]
consistentCrossProduct i1 i2 = [ i | i <- sequence (i1 ++ i2), consistent i]

toNNF :: Expr -> Expr
toNNF (Var p) = Var p
toNNF (Not (Not p)) = (toNNF p)
toNNF (And p q) = And (toNNF p) (toNNF q)
toNNF (Or p q) = Or (toNNF p) (toNNF q)
toNNF (Not (And p q)) = Or (toNNF $ Not p) (toNNF $ Not q)
toNNF (Not (Or p q)) = And (toNNF $ Not p) (toNNF $ Not q)
toNNF (Not p) = Not $ toNNF p

intersectFunc :: [[Interpretation]] -> [[Interpretation]]
intersectFunc [[]] = [[]]
intersectFunc (i:[]) = [i]
intersectFunc (i1:i2:is) = intersectFunc ([(intersect i1 i2)]++is)

-- Exercise A8
consistent :: Interpretation -> Bool
consistent lst = and [ xnorFunc [e2 | (e1, e2) <- lst, token == e1] | token <- (uniqueTokens lst)]

solve :: Expr -> [Interpretation]
solve e = solver $ toNNF e

solver :: Expr -> [Interpretation]
solver (Var s) = [[(s, True)]]
solver (Not (Var s)) = [[(s, False)]]
solver (Or e1 e2) = (solver e1) ++ (solver e2)
solver (And e1 e2) = map nub (consistentCrossProduct (solver e1) (solver e2))

satisfiable :: [Expr] -> Bool
satisfiable [] = True
satisfiable e | intersectFunc (map solve e) /= [[]] = True
              | otherwise = False

main = do
    putStrLn $ "Unique: " ++ show (uniqueTokens [('a',True),('b',True), ('a',True), ('b',False)])
    putStrLn $ "Consistent single True: " ++ show (consistent [('a',True)])
    putStrLn $ "Consistent False: " ++ show (consistent [('a',True),('b',True), ('a',True), ('b',False)])
    putStrLn $ "Consistent True: " ++ show (consistent [('a',True),('b',False), ('a',True), ('b',False), ('b',False)])
    putStrLn $ "toNNF: " ++ show (toNNF (And (Var 'a') (Var 'a')))
    putStrLn $ "toNNF: " ++ show (toNNF (Not (And (Var 'a') (Var 'b'))))
    --putStrLn $ "toNNF: " ++ show (toNNF (Not (Or (Var 'a') (Var 'b'))))
    --putStrLn $ "toNNF: " ++ show (toNNF (Or (Or (Var 'a') (Not (Var 'b'))) (And (Var 'b') (Not (Var 'a')))))
    putStrLn $ "toNNF: " ++ show (toNNF (And (And (And (Var 'a') (Var 'b')) (Or (Not (Var 'a')) (Var 'c'))) (Or (Not (Var 'c')) (Not (Var 'b')))))
    --putStrLn $ "Solve: " ++ show (solve (Or (Var 'a') (Var 'b')))
    putStrLn $ "Solve: " ++ show (solve (And (Var 'a') (Var 'b')))
    putStrLn $ "Solve: " ++ show (solve (Not (And (Var 'a') (Var 'b'))))
    putStrLn $ "Solve: " ++ show (solve (And (And (And (Var 'a') (Var 'b')) (Or (Not (Var 'a')) (Var 'c'))) (Or (Not (Var 'c')) (Not (Var 'b')))))
