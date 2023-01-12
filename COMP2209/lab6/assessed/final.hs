{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (connected,pathFinder,Dir(..),Strategy,insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Put ALL of your own import statements here:
import Data.List
import Data.List (sort)
import Data.Graph hiding (Node)
import Data.Tuple
import Data.Maybe

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

-- Exercise A4
--Utilitarian methods
generatePointsXY1 :: [((Integer,Integer), (Integer,Integer))] -> [(Integer,Integer)]
generatePointsXY1 ppairs = sort (concat [[(fst ppair),(snd ppair)] | ppair <- ppairs ])

getMazeSizeX :: [((Integer,Integer), (Integer,Integer))] -> Integer
getMazeSizeX maze = maximum (fst (unzip (generatePointsXY1 maze)))

getMazeSizeY :: [((Integer,Integer), (Integer,Integer))] -> Integer
getMazeSizeY maze = maximum (snd (unzip (generatePointsXY1 maze)))

mazeSize1 :: [((Integer,Integer), (Integer,Integer))] -> Int
mazeSize1 maze = maximum $ concat [ [fromInteger $ fst (fst x), fromInteger $ fst (snd x), fromInteger $ snd (fst x), fromInteger $ snd (snd x)] | x <- maze]

tupleToListUniq :: Eq a => [(a,a)] -> [a]
tupleToListUniq [] = []
tupleToListUniq ((x,y):xs) = nub (x : y : tupleToListUniq xs)

connected :: [((Integer, Integer), (Integer, Integer))] -> Bool
connected [] = True
connected maze = checkConnected (tupleToListUniq (edges $ graph maze (mazeSize1 maze + 1))) (graph maze (mazeSize1 maze + 1))

checkConnected :: [Int] -> Graph -> Bool
checkConnected [] graph = True
checkConnected [x] graph = True
checkConnected (x:xs) graph = if sort (reachable graph x) == sort (tupleToListUniq (edges $ graph)) then checkConnected xs graph else False

graph :: [((Integer, Integer), (Integer, Integer))] -> Int -> Graph
graph maze size = buildG (0, size * size) edges
    where edges = concat [generateGraphEdges (checkPairLowToHigh1 pair) (toInteger size) | pair <- maze]

checkPairLowToHigh1 :: ((Integer,Integer), (Integer,Integer)) -> ((Integer,Integer), (Integer,Integer))
checkPairLowToHigh1 pair = if fst (fst pair) + snd (fst pair) > fst (snd pair) + snd (snd pair)
    then (snd pair, fst pair)
    else pair

generateGraphEdges :: ((Integer, Integer), (Integer, Integer)) -> Integer -> [(Int, Int)]
generateGraphEdges pair size = if (fst (fst pair)) == (fst (snd pair))
    then [(fromInteger $ fst (fst pair) * size + y, fromInteger $ fst (fst pair) * size + y + 1) | y <- [(snd (fst pair))..(snd (snd pair))-1]] ++ [(fromInteger $ fst (fst pair) * size + y + 1, fromInteger $ fst (fst pair) * size + y) | y <- [(snd (fst pair))..(snd (snd pair))-1]]
    else [(fromInteger $ y * size + snd (fst pair), fromInteger $ (y + 1) * size + snd (fst pair)) | y <- [(fst (fst pair))..(fst (snd pair))-1]] ++ [(fromInteger $ (y + 1) * size + snd (fst pair), fromInteger $ y * size + snd (fst pair)) | y <- [(fst (fst pair))..(fst (snd pair))-1]]


-- Exercise A5

-- Do not modify this datatype
data Dir = U | D | L | R deriving (Eq,Ord,Show,Read)
type Strategy = Dir -> [Dir] -> Maybe Dir

--Utilitarian methods
generatePointsXY :: [((Int,Int), (Int,Int))] -> [(Int,Int)]
generatePointsXY ppairs = nub (concat [generateRange (fst (checkPairLowToHigh ppair)) (snd (checkPairLowToHigh ppair)) | ppair <- ppairs ])

mazeSize :: [((Int,Int), (Int,Int))] -> Int
mazeSize maze = maximum $ concat [ [fst (fst x), fst (snd x), snd (fst x), snd (snd x)] | x <- maze]

tupleSwap :: [(a, b)]->[(b, a)]
tupleSwap = map swap

generateRange :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
generateRange p1 p2 = if (fst p1) == (fst p2)
    then [(fst p1, y) | y <- [(snd p1)..(snd p2)]]
    else [(y, snd p1) | y <- [(fst p1)..(fst p2)]]

checkPairLowToHigh :: ((Int,Int), (Int,Int)) -> ((Int,Int), (Int,Int))
checkPairLowToHigh pair = if fst (fst pair) + snd (fst pair) > fst (snd pair) + snd (snd pair)
    then (snd pair, fst pair)
    else pair

pathFinder :: [((Int,Int),(Int,Int))] -> Strategy -> ((Int,Int),Dir) -> (Int,Int) -> Bool
pathFinder maze strat start end = if checkInMaze (generatePointsXY maze) start
    then walkMaze (generatePointsXY maze) strat (firstStep (generatePointsXY maze) strat start) end
    else False

walkMaze :: [(Int,Int)] -> Strategy -> ((Int,Int), Maybe Dir) -> (Int,Int) -> Bool
walkMaze maze strat (_, Nothing) end = False
walkMaze maze strat pos end = if fst pos /= end
    then walkMaze maze strat (makeStep (fst pos) (strat (fromJust $ snd pos) (getMoveOptions maze (fst pos))),
        getMoveDirection maze strat pos) end else True

checkInMaze :: [(Int,Int)] -> ((Int,Int),Dir) -> Bool
checkInMaze maze point = elem (fst point) maze

getMoveDirection :: [(Int,Int)] -> Strategy -> ((Int,Int), Maybe Dir) -> Maybe Dir
getMoveDirection maze strat start = strat (fromJust $ snd start) (getMoveOptions maze (fst start))

getMoveOptions :: [(Int,Int)] -> (Int,Int) -> [Dir]
getMoveOptions points point = [ snd p | p <- generateAdjentPoints point, (fst p) `elem` points ]

generateAdjentPoints :: (Int,Int) -> [((Int,Int), Dir)]
generateAdjentPoints point = [((fst point, snd point-1), U), ((fst point, snd point+1), D), ((fst point-1, snd point), L), ((fst point+1, snd point), R)]

firstStep :: [(Int,Int)] -> Strategy -> ((Int,Int),Dir) -> ((Int,Int), Maybe Dir)
firstStep maze strat start = (makeStep (fst start) (stratOutput), stratOutput)
    where stratOutput = strat (snd start) (getMoveOptions maze (fst start))

makeStep :: (Int,Int) -> Maybe Dir -> (Int, Int)
makeStep start move = case move of
    Nothing -> start
    Just U -> (fst start, snd start-1)
    Just D -> (fst start, snd start+1)
    Just L -> (fst start-1, snd start)
    Just R -> (fst start+1, snd start)

-- Exercise A6
-- Do not modify this datatype
data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = Lt a Int (VTree a) | Rt a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

--Utilitarian
mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\z -> \x -> insertFromCurrentNode x z) (Leaf,[])

incrementNode :: Ord a => VTree a -> VTree a
incrementNode Leaf = Leaf
incrementNode (Node l x y r) = (Node l x (y+1) r)

goLeft,goRight,goUp :: Ord a => (VTree a, Trail a) -> (VTree a, Trail a)

goLeft (Node l x y r , ts) = (incrementNode l , Lt x y r:ts)
goRight (Node l x y r , ts) = (incrementNode r , Rt x y l:ts)

goUp (t , Lt x y r : ts) = (Node t x (y+1) r , ts)
goUp (t , Rt x y l : ts) = (Node l x (y+1) t , ts)

attach :: Ord a => VTree a -> Zipper a -> Zipper a
attach t ( _ , ts) = (t, ts)

-- Exercise A6
insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode x (Leaf, []) = attach (Node Leaf x 1 Leaf) (Leaf, [])
insertFromCurrentNode x zip = if fst (travelDown x (goRoot x zip)) == Leaf
    then attach (Node Leaf x 1 Leaf) (travelDown x (goRoot x zip))
    else travelDown x (goRoot x zip)

goRoot :: Ord a => a -> Zipper a -> Zipper a
goRoot n (t , []) = (t, [])
goRoot n (Node l1 x1 y1 r1, (Rt x2 y2 r2:ts)) =
    if x1==n
        then (Node l1 x1 y1 r1, (Rt x2 y2 r2:ts))
    else if n > x2 && n < x1
        then goUp (Node l1 x1 y1 r1, (Rt x2 y2 r2:ts))
    else goRoot n (goUp (Node l1 x1 y1 r1, (Rt x2 y2 r2:ts)))
goRoot n (Node l1 x1 y1 r1, (Lt x2 y2 r2:ts)) =
    if x1==n
        then (Node l1 x1 y1 r1, (Lt x2 y2 r2:ts))
    else if n < x2 && n > x1
        then goUp (Node l1 x1 y1 r1, (Lt x2 y2 r2:ts))
    else goRoot n (goUp (Node l1 x1 y1 r1, (Lt x2 y2 r2:ts)))

travelDown :: Ord a => a -> Zipper a -> Zipper a
travelDown n (Leaf, ts) = (Leaf, ts)
travelDown n (Node l x y r, ts) =
    if n == x
        then (Node l x y r, ts)
    else if n < x
        then travelDown n (goLeft (Node l x y r, ts))
    else travelDown n (goRight (Node l x y r, ts))