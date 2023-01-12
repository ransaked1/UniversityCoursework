{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

--module Exercises (Expr(..),toNNF) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Exercise A7
data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord, Show, Read)

-- Define toNNF here:
toNNF :: Expr -> Expr
toNNF (Var e) = (Var e)

toNNF (Not (Var e)) = (Not (Var e))
toNNF (And e1 e2) = (And (toNNF e1) (toNNF e2))
toNNF (Or e1 e2) = (Or (toNNF e1) (toNNF e2))

toNNF (Not (Not e)) = toNNF e
toNNF (Not (Or e1 e2)) = (And (toNNF (Not e1)) (toNNF (Not e2)))
toNNF (Not (And e1 e2)) = (Or (toNNF (Not e1)) (toNNF (Not e2)))

main = do
  print $ "Test1: " ++ show (toNNF (Var 'a'))
  print $ "Test2: " ++ show (toNNF (Not (Not (Var 'a'))))
  print $ "Test3: " ++ show (toNNF (Not (And (Var 'a') (Var 'b'))))
  print $ "Test4: " ++ show (toNNF (Not (Or (Not (And (Not (Var 'a')) (Var 'b'))) (And (Not (Var 'b')) (Var 'a')))))
  print $ "Test5: " ++ show (toNNF (Not (Or (Or (And (Var 'a') (Var 'b')) (Or (Not (Var 'a')) (Var 'c'))) (Or (Not (Var 'c')) (Not (Var 'b'))))))