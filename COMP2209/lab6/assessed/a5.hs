{-# LANGUAGE DeriveGeneric #-}

--TEMPLATE FILE FOR COURSEWORK 2 for COMP2209
--Julian Rathke, Oct 2022

--EXERCISE A5 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

--module Exercises (pathFinder,Dir(..),Strategy) where

-- Extra imports needed
import Data.List
import Data.List (sort)
import Data.Tuple
import Data.Maybe

data Dir = U | D | L | R deriving (Eq,Ord,Show,Read)
type Strategy = Dir -> [Dir] -> Maybe Dir

--Strategy definition to use
strategy1 :: Strategy
strategy1 D [U,D,L,R] = Just R
strategy1 D [U,D] = Just D
strategy1 D [D] = Just D
strategy1 D [U,R] = Just R
strategy1 R [U,L,R] = Just U
strategy1 R [U,D,L,R] = Just U
strategy1 R [L,R] = Just R
strategy1 U [D,L,R] = Just R
strategy1 U [U,D,L] = Just U
strategy1 U [U,D] = Just U
strategy1 _ _ = Nothing

strategy2 :: Strategy
strategy2 D [D] = Just D
strategy2 D [U,D] = Just D
strategy2 D [U,R] = Just R
strategy2 R [L,R] = Just R
strategy2 _ _ = Nothing

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

main = do
    let maze1 = [
         ((1,1),(1,16)),
         ((0,14),(25,14)),
         ((15,14),(15,10)),
         ((13,10),(25,10)),
         ((20,0),(20,12)),
         ((5,5),(20,5))]
    let maze2 = [((0,0),(0,3)), ((1,3),(3,3))]
    putStrLn $ "generateXY: " ++ show (generatePointsXY maze1)
    putStrLn $ "Move options: " ++ show (getMoveOptions (generatePointsXY maze1) (1,1))
    putStrLn $ "Strat output: " ++ show (strategy1 D (getMoveOptions (generatePointsXY maze1) (1,1)))
    putStrLn $ "First move: " ++ show (firstStep (generatePointsXY maze1) strategy1 ((1,1),D))
    putStrLn $ "Solve: " ++ show (pathFinder maze1 strategy1 ((1,1),D) (20,5))
    putStrLn $ "generateXY: " ++ show (generatePointsXY maze2)
    putStrLn $ "Move options: " ++ show (getMoveOptions (generatePointsXY maze2) (0,0))
    putStrLn $ "First move: " ++ show (firstStep (generatePointsXY maze2) strategy2 ((0,0),D))
    putStrLn $ "Solve: " ++ show (pathFinder maze2 strategy2 ((0,0),D) (3,3))
