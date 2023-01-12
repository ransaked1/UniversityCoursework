{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (Expr(..),toNNF,Binding,Interpretation,consistent,solve,satisfiable,maxSatisfiable) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

--Additional imports needed
import Data.List
import Data.Ord

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

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

longest = maximumBy (comparing length)

generateSatisfiableSubsets :: [Expr] -> [[Expr]]
generateSatisfiableSubsets e = [subset | subset <- (subsets e), satisfiable subset]

solve2 :: Expr -> [Interpretation]
solve2 e = [ i | i <- consistentTokenSubsets e, solver2 e i]

solver2 :: Expr -> Interpretation -> Bool
solver2 (Var a) i = getFromInterpretation a i
solver2 (Not e) i = not (solver2 e i)
solver2 (And e1 e2) i = (solver2 e1 i) && (solver2 e2 i)
solver2 (Or e1 e2) i = (solver2 e1 i) || (solver2 e2 i)

-- Exercise A7
data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord, Show, Read)

toNNF :: Expr -> Expr
toNNF (Var e) = (Var e)

toNNF (Not (Var e)) = (Not (Var e))
toNNF (And e1 e2) = (And (toNNF e1) (toNNF e2))
toNNF (Or e1 e2) = (Or (toNNF e1) (toNNF e2))

toNNF (Not (Not e)) = toNNF e
toNNF (Not (Or e1 e2)) = (And (toNNF (Not e1)) (toNNF (Not e2)))
toNNF (Not (And e1 e2)) = (Or (toNNF (Not e1)) (toNNF (Not e2)))

-- Exercise A8
type Binding = (Char, Bool)
type Interpretation = [Binding]

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
satisfiable e | intersectFunc (map solve2 e) /= [[]] = True
              | otherwise = False

-- Exercise A9
maxSatisfiable :: [Expr] -> [[Expr]]
maxSatisfiable e = [ sub | sub <- generateSatisfiableSubsets e, length (longest (generateSatisfiableSubsets e)) == length sub]