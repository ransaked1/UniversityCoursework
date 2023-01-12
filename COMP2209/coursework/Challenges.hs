{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2022
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),ArithExpr(..),
                   calcBBInteractions,solveBB,prettyPrint,
                   parseArith,churchEnc,innerRedn1,innerArithRedn1,compareArithLam,listEquals,toANF) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Extra imports
import Data.Maybe
import Data.List

instance NFData ArithExpr
instance NFData LamExpr
instance NFData Marking
instance NFData Side

-- Challenge 1
-- Calculate Interactions in the Black Box

type Atoms = [ Pos ]
type Interactions = [  ( EdgePos , Marking )  ]
type Pos = (Int, Int)   -- top left is (1,1) , bottom right is (N,N) where N is size of grid
type EdgePos = ( Side , Int ) -- int range is 1 to N where N is size of grid

data Side = North | East | South | West
            deriving (Show, Eq, Ord, Generic)

data Marking =  Absorb | Reflect | Path EdgePos
                deriving (Show, Eq)

--Extra structures
--The Move structure will hold the positon and direction of movement
type Move = (Pos, Side)

--Utility function used for comparing lists regardless of element ordering
--1.Source: https://stackoverflow.com/questions/23978834/check-if-two-lists-have-the-same-elements
listEquals :: (Eq a) => [a] -> [a] -> Bool
listEquals x y = null (x \\ y) && null (y \\ x)

--Convertion functions
edgePosToPos :: Int -> EdgePos -> Move
edgePosToPos n (North, coord) = ((coord, 0), South)
edgePosToPos n (West, coord) = ((0, coord), East)
edgePosToPos n (South, coord) = ((coord, n+1), North)
edgePosToPos n (East, coord) = ((n+1, coord), West)

posToEdgePos :: Int -> Pos -> Marking
posToEdgePos n (x, y) | y == 0 = Path (North, x)
                    | x == 0 = Path (West, y)
                    | y == n+1 = Path (South, x)
                    | otherwise = Path (East, y)

--Generate the positions to check if there are atoms there
generateCheckPositions :: Move -> [Pos]
generateCheckPositions ((x, y), North) = [(x-1, y-1), (x, y-1), (x+1, y-1)]
generateCheckPositions ((x, y), South) = [(x-1, y+1), (x, y+1), (x+1, y+1)]
generateCheckPositions ((x, y), West) = [(x-1, y-1), (x-1, y), (x-1, y+1)]
generateCheckPositions ((x, y), East) = [(x+1, y-1), (x+1, y), (x+1, y+1)]

--Function is the intersection of two sets
--2.Source: https://stackoverflow.com/questions/69442683/how-do-i-find-the-intersection-of-two-lists-in-haskell
findAtoms :: [Pos] -> [Pos] -> [Pos]
findAtoms [] _ = []
findAtoms _ [] = []
findAtoms xs ys = filter (\x -> x `elem` xs) ys

--Atoms to check depending on position and movement direction
getRelevantAtoms :: Atoms -> Move -> Atoms
getRelevantAtoms atoms point = findAtoms atoms (generateCheckPositions point)

--Any adjent atom is an absorb
absorbCheck :: Move -> Atoms -> Bool
absorbCheck ((x, y), _) atoms = findAtoms [(x, y+1), (x, y-1), (x-1, y), (x+1, y)] atoms /= []

--If any atom is on the corner it is a reflection
reflectCheckEdge :: Move -> Atoms -> Bool
reflectCheckEdge ((x, y), _) atoms = findAtoms [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)] atoms /= []

--If two atoms are in the corner then it is a reflection
reflectCheckInBox :: Move -> Atoms -> Bool
reflectCheckInBox ((x, y), _) atoms = length (findAtoms [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)] atoms) == 2

--If one atom is in the corner then it is a direction change
directionChangeCheck :: Move -> Atoms -> Bool
directionChangeCheck ((x, y), _) atoms = length (findAtoms [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)] atoms) == 1

--For one atom in coner cases get the atom
getCornerAtom :: Move -> Atoms -> Pos
getCornerAtom ((x, y), _) atoms = head (findAtoms [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)] atoms)

--Checking if there is a result without moving
edgePreProcessing :: Int -> Atoms -> EdgePos -> Maybe (EdgePos , Marking)
edgePreProcessing n atoms edgePosition
    | absorbCheck pos (getRelevantAtoms atoms pos)      = Just (edgePosition, Absorb)
    | reflectCheckEdge pos (getRelevantAtoms atoms pos) = Just (edgePosition, Reflect)
    | otherwise = Nothing
    where pos = edgePosToPos n edgePosition

makeStep :: Move -> Move
makeStep ((x, y), North) = ((x, y-1), North)
makeStep ((x, y), South) = ((x, y+1), South)
makeStep ((x, y), West) = ((x-1, y), West)
makeStep ((x, y), East) = ((x+1, y), East)

atEdgeCheck :: Int -> Move -> Bool
atEdgeCheck n ((x, y), _) = x == 0 || y == 0 || x == n+1 || y == n+1

--Change movement direction
changeDir :: Move -> Atoms -> Move
changeDir ((x, y), North) atoms | getCornerAtom ((x, y), North) (getRelevantAtoms atoms ((x, y), North)) == (x+1, y-1) = ((x, y), West)
                                | otherwise = ((x, y), East)
changeDir ((x, y), South) atoms | getCornerAtom ((x, y), South) (getRelevantAtoms atoms ((x, y), South)) == (x+1, y+1) = ((x, y), West)
                                | otherwise = ((x, y), East)
changeDir ((x, y), East) atoms | getCornerAtom ((x, y), East) (getRelevantAtoms atoms ((x, y), East)) == (x+1, y-1) = ((x, y), South)
                                | otherwise = ((x, y), North)
changeDir ((x, y), West) atoms | getCornerAtom ((x, y), West) (getRelevantAtoms atoms ((x, y), West)) == (x-1, y-1) = ((x, y), South)
                                | otherwise = ((x, y), North)

--Walk the ray path
moveInBox :: Int -> Atoms -> Move -> Marking
moveInBox n atoms point
    | atEdgeCheck n point = (posToEdgePos n (fst point))
    | absorbCheck point relevantAtoms = Absorb
    | reflectCheckInBox point relevantAtoms = Reflect
    | directionChangeCheck point relevantAtoms
        = moveInBox n atoms (makeStep (changeDir point relevantAtoms))
    | otherwise = moveInBox n atoms (makeStep point)
    where relevantAtoms = getRelevantAtoms atoms point

calcBBInteraction :: Int -> Atoms -> EdgePos -> (EdgePos , Marking)
calcBBInteraction n atoms edgePosition
    | edgePreProcessing n atoms edgePosition == Nothing
        = (edgePosition, moveInBox n atoms (makeStep $ edgePosToPos n edgePosition))
    | otherwise = fromJust (edgePreProcessing n atoms edgePosition)

calcBBInteractions :: Int -> Atoms -> [EdgePos] -> Interactions
calcBBInteractions n atoms edgePositions = map (\x -> calcBBInteraction n atoms x) edgePositions

-- Challenge 2
-- Find atoms in a Black Box

--Functions to find the cartesian product of N lists
--3.Source: https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Haskell
cartProdN :: [[a]] -> [[a]]
cartProdN = sequence

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = (,) <$> xs <*> ys

--Brute force generate all the possible single atom positions
genAllPositionsForAtom :: Int -> Atoms
genAllPositionsForAtom n = [(x,y) | x <- [1..n], y <- [1..n]]

--Brute force generate all the possible multiple atom positions
genAllForCount :: Int -> Int -> [Atoms]
genAllForCount n 0 = [[]]
genAllForCount n c = map nub (cartProdN [genAllPositionsForAtom n | i <- [1..c]])

getEdgePosFromInter :: Interactions -> [EdgePos]
getEdgePosFromInter is = [ edgepos | (edgepos, _) <- is]

--Test all the given atom configurations for an atom count until a valid one is found
checkBBForAtomCount :: Int -> Interactions -> [EdgePos] -> Int -> [Atoms] -> Maybe Atoms
checkBBForAtomCount n i ep c [] = Nothing
checkBBForAtomCount n i ep c (a:as) | listEquals i (calcBBInteractions n a ep) = Just a
                                    | otherwise = checkBBForAtomCount n i ep c as

solveBBForAtomCount :: Int -> Interactions -> Int -> Atoms
solveBBForAtomCount n i count | result == Nothing = solveBBForAtomCount n i (count+1)
                              | otherwise = fromJust result
                              where result = checkBBForAtomCount n i (getEdgePosFromInter i) count (genAllForCount n count)

solveBB :: Int -> Interactions -> Atoms
solveBB n i = solveBBForAtomCount n i 0

-- Challenge 3
-- Pretty Printing Lambda with Alpha-Normalisation

data LamExpr =  LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int
                deriving (Eq, Show, Read)

--Check lambda variable is free
--4.Source: Function adapted from Lecture 38 slides
checkFree :: LamExpr -> Int -> Bool
checkFree (LamVar t) n = t == n
checkFree (LamAbs t e) n | t /= n       = checkFree e n
                         | otherwise    = False
checkFree (LamApp e1 e2) n = (checkFree e1 n) || (checkFree e2 n)

--Check lambda variable is bound
checkBound :: LamExpr -> Int -> Bool -> Bool
checkBound (LamVar t) n bool | t == n && bool   = True
                             | otherwise        = False
checkBound (LamAbs t e) n bool | t == n || bool = checkBound e n True
                               | otherwise      = checkBound e n False
checkBound (LamApp e1 e2) n bool = (checkBound e1 n bool) || (checkBound e2 n bool)

--Get next free variable
getNextVar :: LamExpr -> Int -> Int
getNextVar e n | checkFree e n = getNextVar e (n+1)
               | otherwise = n

--Convert variable to lambda
convert :: LamExpr -> Int -> Int -> LamExpr
convert (LamVar t) n m | t == m     = LamVar n
                       | otherwise  = LamVar t
convert (LamAbs t e) n m | t == n       = LamAbs m (convert e n m)
                         | t == m       = LamAbs n (convert e n m)
                         | otherwise    = LamAbs t (convert e n m)
convert (LamApp e1 e2) n m = LamApp (convert e1 n m) (convert e2 n m)

--Convert to Alpha Normal Form
toANF :: LamExpr -> Int -> LamExpr
toANF (LamVar t) n = LamVar t
toANF (LamAbs t e) n | t == n                                   = LamAbs t (toANF e n)
                     | checkBound (LamAbs t e) t False          = LamAbs n (toANF (convert e n t) n)
                     | checkBound (LamAbs t e) t False && checkBound (LamAbs t e) n False
                                                                = LamAbs n (toANF (convert (convert e free n) n t) n)
                     | checkBound (LamAbs t e) n False == False = LamAbs free (toANF (convert e free t) free)
                     | otherwise                                = LamAbs n (toANF e n)
                     where free = getNextVar (LamAbs t e) n
toANF (LamApp e1 e2) n | checkFree e1 0 && checkFree e2 0   = LamApp (toANF e1 free) (toANF e2 free)
                       | otherwise                          = LamApp (toANF e1 0) (toANF e2 0)
                       where free = getNextVar (LamApp e1 e2) 0

absCount :: LamExpr -> Int -> Int
absCount (LamVar t) n = n --No change
absCount (LamAbs t e) n = absCount e (n+1) --Count abstraction
absCount (LamApp e1 e2) n = absCount e1 n + absCount e2 n --Add up abstraction counts

exprWithAbs :: LamExpr -> Int -> Int
exprWithAbs e n = n

printExpr :: LamExpr -> Int -> String
printExpr (LamVar t) n = "x" ++ (show t)
printExpr (LamAbs t e) n = "\\x" ++ (show t) ++ " -> " ++ (printExpr e (n-1)) --Continue with one less abstraction
printExpr (LamApp (LamAbs t e) (LamApp e1 e2)) n = "(" ++ (printExpr (LamAbs t e) n) ++ ") (" ++ (printExpr (LamApp e1 e2) (absCount (LamApp e1 e2) 0))
printExpr (LamApp (LamAbs t1 e1) (LamAbs t2 e2)) n | n - (absCount (LamAbs t1 e1) 0) == absCount (LamAbs t2 e2) 0
                                                                = "(" ++ (printExpr (LamAbs t1 e1) n) ++ ") " ++ (printExpr (LamAbs t2 e2) (n - (absCount (LamAbs t1 e1) 0)))
                                                   | otherwise  = "(" ++ (printExpr (LamAbs t1 e1) n) ++ ") (" ++ (printExpr (LamAbs t2 e2) (n - (absCount (LamAbs t1 e1) 0))) ++ ")"
printExpr (LamApp ex (LamApp e1 e2)) n = (printExpr ex n) ++ " (" ++ (printExpr (LamApp e1 e2) (absCount (LamApp e1 e2) 0)) ++ ")"
printExpr (LamApp ex (LamAbs t e)) n  = (printExpr ex n) ++ " " ++ (printExpr (LamAbs t e) (n - (absCount ex 0)))
printExpr (LamApp (LamAbs t e) ex) n = "(" ++ (printExpr (LamAbs t e) n) ++ ") " ++ (printExpr ex (n - (absCount (LamAbs t e) 0)))
printExpr (LamApp e1 e2) n = (printExpr e1 n) ++ " " ++ (printExpr e2 (n - (absCount e1 0)))

prettyPrint :: LamExpr -> String
prettyPrint e = printExpr (toANF e 0) (absCount (toANF e 0) 0)

-- Challenge 4
-- Parsing Arithmetic Expressions

data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr
               | Section ArithExpr  | SecApp ArithExpr ArithExpr | ArithNum Int
    deriving (Show,Eq,Read)

--5.Source: Parsers adapted from Lecture 45 slides
multArithExpr :: Parser (Maybe ArithExpr)
multArithExpr = do e1 <- expr1
                   symbol "*"
                   e2 <- expr2
                   return (Just (Mul (fromJust e1) (fromJust e2)))

addArithExpr :: Parser (Maybe ArithExpr)
addArithExpr = do e1 <- expr2
                  symbol "+"
                  e2 <- expr1
                  return (Just (Add (fromJust e1) (fromJust e2)))

numArithExpr :: Parser (Maybe ArithExpr)
numArithExpr = do n <- nat
                  return (Just (ArithNum n))

secArithExpr :: Parser (Maybe ArithExpr)
secArithExpr = do symbol "("
                  symbol "+"
                  e1 <- expr
                  symbol ")"
                  e2 <- expr2
                  return (Just (SecApp (Section (fromJust e1)) (fromJust e2)))

bracketArithExpr :: Parser (Maybe ArithExpr)
bracketArithExpr = do symbol "("
                      e <- expr
                      symbol ")"
                      return (Just (fromJust e))

expr :: Parser (Maybe ArithExpr)
expr = multArithExpr <|> expr1
expr1 = addArithExpr <|> expr2
expr2 = numArithExpr <|> secArithExpr <|> bracketArithExpr

--Removing whitespace from input string
clean s = filter (\xs -> (xs /=' ')) s

--Return parsed expression or discard if nohting was parsed or there is unparsed leftover
parseArith :: String -> Maybe ArithExpr
parseArith s | length (parse expr s) == 0 || (clean $ snd (head (parse expr s))) /= ""  = Nothing
             | otherwise                                                                = fst (head (parse expr s))

-- Challenge 5
-- Church Encoding of arithmetic

numToLamApp :: LamExpr -> Int -> LamExpr
numToLamApp e n | n == 0 = e
                | otherwise = numToLamApp (LamApp (LamVar 0) e) (n-1)

-- λm -> λn -> ( λf -> λx -> m f ( n f x ) )
addExpr = (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))))

-- λm -> λn -> ( λf -> λx -> m (n f) x )
mulExpr = (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3))))))

churchEnc :: ArithExpr -> LamExpr
churchEnc (ArithNum n) = (LamAbs 0 (LamAbs 1 (numToLamApp (LamVar 1) n)))
churchEnc (Add e1 e2) = (LamApp (LamApp (addExpr) (churchEnc e1)) (churchEnc e2))
churchEnc (SecApp (Section e1) e2) = (LamApp (LamApp (addExpr) (churchEnc e1)) (churchEnc e2))
churchEnc (Mul e1 e2) = (LamApp (LamApp (mulExpr) (churchEnc e1)) (churchEnc e2))

-- Challenge 6
-- Compare Innermost Reduction for Arithmetic and its Church Encoding

innerRedn1 :: LamExpr -> Maybe LamExpr
innerRedn1 e | e /= reduced = Just reduced
             | otherwise = Nothing
             where reduced = lamRedn e

--4.Source: Function adapted from Lecture 38 slides
lamRedn :: LamExpr -> LamExpr
lamRedn (LamVar t) = LamVar t --No reduction under λ
lamRedn (LamAbs t e) = LamAbs t (lamRedn e)
lamRedn (LamApp (LamAbs t e1) e2) | (LamAbs t e1) == lamRedn (LamAbs t e1) = sub e1 t e2
                                  | otherwise                   = LamApp (lamRedn (LamAbs t e1)) e2
lamRedn (LamApp e1 e2) | e1 == lamRedn e1   = LamApp e1 (lamRedn e2)
                       | otherwise          = LamApp (lamRedn e1) e2

--4.Source: Function adapted from Lecture 38 slides for beta reduction
sub :: LamExpr -> Int -> LamExpr -> LamExpr
sub (LamVar n) t e | n /= t     = LamVar n
                   | otherwise  = e
sub (LamAbs n e1) t e2 | n /= t && (checkFree e2 n) = sub (LamAbs newVal (sub e1 n (LamVar newVal))) t e2
                       | n /= t && not (checkFree e2 n) = LamAbs n (sub e1 t e2)
                       | otherwise = LamAbs n e1
                       where newVal = (max n t) + 1
sub (LamApp e1 e2) t e = LamApp (sub e1 t e) (sub e2 t e)

innerArithRedn1 :: ArithExpr -> Maybe ArithExpr
innerArithRedn1 e | e /= reduced = Just reduced
                  | otherwise = Nothing
                  where reduced = arithRedn e

arithRedn :: ArithExpr -> ArithExpr
arithRedn (ArithNum e) = ArithNum e

arithRedn (Add (ArithNum n1) (ArithNum n2)) = ArithNum (n1 + n2)
arithRedn (Add e (ArithNum n)) = (Add (arithRedn e) (ArithNum n))
arithRedn (Add (ArithNum n) e) = (Add (ArithNum n) (arithRedn e))
arithRedn (Add e1 e2) = (Add (arithRedn e1) (arithRedn e2))

arithRedn (Mul (ArithNum n1) (ArithNum n2)) = ArithNum (n1 * n2)
arithRedn (Mul e (ArithNum n)) = (Add (arithRedn e) (ArithNum n))
arithRedn (Mul (ArithNum n) e) = (Add (ArithNum n) (arithRedn e))
arithRedn (Mul e1 e2) = (Add (arithRedn e1) (arithRedn e2))

arithRedn (SecApp (Section (ArithNum n1)) (ArithNum n2)) = ArithNum (n1 + n2)
arithRedn (SecApp (Section e) (ArithNum n)) = (SecApp (Section (arithRedn e)) (ArithNum n))
arithRedn (SecApp (Section (ArithNum n)) e) = (SecApp (Section (ArithNum n)) (arithRedn e))
arithRedn (SecApp (Section e1) e2) = (SecApp (Section (arithRedn e1)) (arithRedn e2))

compareArithLam :: ArithExpr -> (Int,Int)
compareArithLam e = (reduce innerArithRedn1 e 0 , reduce innerRedn1 (churchEnc e) 0)

reduce :: (Eq a) => (a -> Maybe a) -> a -> Int -> Int
reduce redn1 e n | redn1 e == Nothing = n
                 | otherwise = reduce redn1 (fromJust $ redn1 e) (n + 1)