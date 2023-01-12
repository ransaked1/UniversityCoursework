{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

--module Exercises (Expr(..), maxSatisfiable) where

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

getAllBindings :: Expr -> [Binding]
getAllBindings e = [ (token, bool) | token <- (nub $ getAllTokens e []), bool <- [True, False]]

getAllTokens :: Expr -> [Char] -> [Char]
getAllTokens (Var a) acc = [a]
getAllTokens (Not e) acc = getAllTokens e acc
getAllTokens (And e1 e2) acc = acc ++ (getAllTokens e1 acc) ++ (getAllTokens e2 acc)
getAllTokens (Or e1 e2) acc = acc ++ (getAllTokens e1 acc) ++ (getAllTokens e2 acc)

getAllTokensCount :: Expr -> Int
getAllTokensCount e = length (nub $ getAllTokens e [])

consistentTokenSubsets :: Expr -> [Interpretation]
consistentTokenSubsets e = [sub | sub <- subsets (getAllBindings e), (consistent sub) && sub /= [] && getAllTokensCount e == length sub]

getFromInterpretation :: Char -> Interpretation -> Bool
getFromInterpretation c i | [snd bind | bind <- i, c == fst bind] /= [] = head [snd bind | bind <- i, c == fst bind]

-- Exercise A8
consistent :: Interpretation -> Bool
consistent lst = and [ xnorFunc [e2 | (e1, e2) <- lst, token == e1] | token <- (uniqueTokens lst)]

solve :: Expr -> [Interpretation]
solve e = [ i | i <- consistentTokenSubsets e, solver e i]

solver :: Expr -> Interpretation -> Bool
solver (Var a) i = getFromInterpretation a i
solver (Not e) i = not (solver e i)
solver (And e1 e2) i = (solver e1 i) && (solver e2 i)
solver (Or e1 e2) i = (solver e1 i) || (solver e2 i)

satisfiable :: [Expr] -> Bool
satisfiable [] = True
satisfiable e | intersectFunc (map solve e) /= [[]] = True
              | otherwise = False

-- Exercise A9
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

longest = maximumBy (comparing length)

generateSatisfiableSubsets :: [Expr] -> [[Expr]]
generateSatisfiableSubsets e = [subset | subset <- (subsets e), satisfiable subset]

maxSatisfiable :: [Expr] -> [[Expr]]
maxSatisfiable e = [ sub | sub <- generateSatisfiableSubsets e, length (longest (generateSatisfiableSubsets e)) == length sub]

main = do
    putStrLn $ "Satisfiable: " ++ show (map solve [(And (Var 'a') (Var 'b')), (Or (Not (Var 'a')) (Var 'b'))])
    putStrLn $ "Subsets: " ++ show (subsets [(And (Var 'a') (Var 'b')),(Or (Not (Var 'a')) (Var 'b'))])
    --putStrLn $ "Satisfiable: " ++ show (satisfiable  [(And (Var 'a') (Var 'b')),(And (Not (Var 'a')) (Not (Var 'b')))])
    putStrLn $ "AllSubsets: " ++ show (consistentTokenSubsets (And (Not (Var 'a')) (Not (Var 'b'))))
    putStrLn $ "Satifiable subsets: " ++ show (generateSatisfiableSubsets [(And (Var 'a') (Var 'b')),(Or (Not (Var 'a')) (Var 'b'))])
    putStrLn $ "Max subsets: " ++ show (maxSatisfiable [(And (Var 'a') (Var 'b')),(Or (Not (Var 'a')) (Var 'b'))])
