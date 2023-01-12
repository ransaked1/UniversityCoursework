{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 2 for COMP2209
--Julian Rathke, Oct 2022

--EXERCISE A4 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

--module Exercises (connected) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Extra imports needed
import Data.List
import Data.List (sort)
import Data.Graph
import Data.Tuple

--Utilitarian methods
generatePointsXY :: [((Integer,Integer), (Integer,Integer))] -> [(Integer,Integer)]
generatePointsXY ppairs = sort (concat [[(fst ppair),(snd ppair)] | ppair <- ppairs ])

getMazeSizeX :: [((Integer,Integer), (Integer,Integer))] -> Integer
getMazeSizeX maze = maximum (fst (unzip (generatePointsXY maze)))

getMazeSizeY :: [((Integer,Integer), (Integer,Integer))] -> Integer
getMazeSizeY maze = maximum (snd (unzip (generatePointsXY maze)))

mazeSize :: [((Integer,Integer), (Integer,Integer))] -> Int
mazeSize maze = maximum $ concat [ [fromInteger $ fst (fst x), fromInteger $ fst (snd x), fromInteger $ snd (fst x), fromInteger $ snd (snd x)] | x <- maze]

tupleToListUniq :: Eq a => [(a,a)] -> [a]
tupleToListUniq [] = []
tupleToListUniq ((x,y):xs) = nub (x : y : tupleToListUniq xs)

-- Exercise A4
connected :: [((Integer, Integer), (Integer, Integer))] -> Bool
connected [] = True
connected maze = checkConnected (tupleToListUniq (edges $ graph maze (mazeSize maze + 1))) (graph maze (mazeSize maze + 1))

checkConnected :: [Int] -> Graph -> Bool
checkConnected [] graph = True
checkConnected [x] graph = True
checkConnected (x:xs) graph = if sort (reachable graph x) == sort (tupleToListUniq (edges $ graph)) then checkConnected xs graph else False

graph :: [((Integer, Integer), (Integer, Integer))] -> Int -> Graph
graph maze size = buildG (0, size * size) edges
    where edges = concat [generateGraphEdges (checkPairLowToHigh pair) (toInteger size) | pair <- maze]

checkPairLowToHigh :: ((Integer,Integer), (Integer,Integer)) -> ((Integer,Integer), (Integer,Integer))
checkPairLowToHigh pair = if fst (fst pair) + snd (fst pair) > fst (snd pair) + snd (snd pair)
    then (snd pair, fst pair)
    else pair

generateGraphEdges :: ((Integer, Integer), (Integer, Integer)) -> Integer -> [(Int, Int)]
generateGraphEdges pair size = if (fst (fst pair)) == (fst (snd pair))
    then [(fromInteger $ fst (fst pair) * size + y, fromInteger $ fst (fst pair) * size + y + 1) | y <- [(snd (fst pair))..(snd (snd pair))-1]] ++ [(fromInteger $ fst (fst pair) * size + y + 1, fromInteger $ fst (fst pair) * size + y) | y <- [(snd (fst pair))..(snd (snd pair))-1]]
    else [(fromInteger $ y * size + snd (fst pair), fromInteger $ (y + 1) * size + snd (fst pair)) | y <- [(fst (fst pair))..(fst (snd pair))-1]] ++ [(fromInteger $ (y + 1) * size + snd (fst pair), fromInteger $ y * size + snd (fst pair)) | y <- [(fst (fst pair))..(fst (snd pair))-1]]

main = do
    let maze1 = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)), ((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)), ((1,5),(4,5))]
    let maze2 = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)), ((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)), ((3,5),(4,5))]
    let maze3 = [((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2)),((2,2),(2,3)),((2,3),(3,3)),((3,3),(3,5)),((3,4),(4,4)),((2,5),(5,5)),((2,5),(2,6)),((1,6),(2,6)),((1,7),(1,6)),((0,7),(1,7)),((5,5),(5,6)),((5,6),(6,6)),((6,6),(6,7)),((6,7),(7,7))]
    --print $ "Connected True: " ++ (show.edges) (graph maze3 (mazeSize maze3 + 1))
    print $ "Connected True: " ++ show (connected maze1)
    print $ "Connected False: " ++ show (connected maze2)
    print $ "Connected True: " ++ show (connected maze3)