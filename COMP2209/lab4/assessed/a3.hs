{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2022

--EXERCISE A3 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

--module Exercises (renderMaze) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Extra imports needed
import Data.List
import Data.Tuple

-- Exercise A3
renderMaze :: [((Integer,Integer), (Integer,Integer))] -> [String]
renderMaze [] = []
renderMaze maze = [generateLine line (getMazeSizeX maze) | line <- generateXReplacement (generatePointsYX maze) (getMazeSizeX maze) (getMazeSizeY maze)]

generatePointsYX :: [((Integer,Integer), (Integer,Integer))] -> [(Integer,Integer)]
generatePointsYX ppairs = sort $ tupleSwap (concat [generateRange (fst ppair) (snd ppair) | ppair <- ppairs ])

generateRange :: (Integer,Integer) -> (Integer,Integer) -> [(Integer,Integer)]
generateRange p1 p2 = if (fst p1) == (fst p2)
    then [(fst p1, y) | y <- [(snd p1)..(snd p2)]]
    else [(y, snd p1) | y <- [(fst p1)..(fst p2)]]

tupleSwap :: [(a, b)]->[(b, a)]
tupleSwap = map swap

generateXReplacement :: [(Integer,Integer)] -> Integer -> Integer -> [[Integer]]
generateXReplacement yxpoints sizeX sizeY = [rmdups [snd p | p <- lst] | lst <- [[(y, x) | x <- [0..sizeX], elem (y, x) yxpoints] | y <- [0..sizeY]]]

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

generateLine :: [Integer] -> Integer -> String
generateLine l n = [ if elem x l then '#' else ' ' | x <- [0..n]]

getMazeSizeX :: [((Integer,Integer), (Integer,Integer))] -> Integer
getMazeSizeX maze = maximum (snd (unzip (generatePointsYX maze)))

getMazeSizeY :: [((Integer,Integer), (Integer,Integer))] -> Integer
getMazeSizeY maze = maximum (fst (unzip (generatePointsYX maze)))

tupleToList :: [(a,a)] -> [a]
tupleToList ((a,b):xs) = a : b : tupleToList xs
tupleToList _ = []

main = do
    --putStrLn $ "generateYX: " ++ show (generatePointsYX [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)),((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)),((1,5),(4,5))])
    let maze = [((0,0),(0,1)), ((0,2),(3,2))]
    --putStrLn $ "sizeX: " ++ show (getMazeSizeX maze)
    --putStrLn $ "sizeY: " ++ show (getMazeSizeY maze)
    putStrLn $ "generateXRep: " ++ show (generateXReplacement (generatePointsYX maze) (getMazeSizeX maze) (getMazeSizeY maze))
    putStrLn $ "final: " ++ show (renderMaze  maze)
