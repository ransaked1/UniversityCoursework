{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 1 OF COURSEWORK 1 for COMP2209, 2021
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2022

module Exercises (vigenere,frequency,renderMaze) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Additional
import Data.Char
import Data.List
import Data.Tuple

-- Exercise A1
vigenere :: String -> (String -> String, String -> String)
vignere "" = (empty, empty)
vigenere key = (encrypt key, decrypt key)

removeWhiteSpace :: String -> String
removeWhiteSpace s = filter (\x -> x /= ' ') s

removeNonAlpha :: String -> String
removeNonAlpha s = filter (`elem` ['A'..'z']) s

cleanInput :: String -> String
cleanInput s = map toUpper (removeNonAlpha (removeWhiteSpace s))

circularKey :: String -> Int -> String
circularKey s len = if length s < len then circularKey (s++s) len else take len s

encrypt :: String -> (String -> String)
encrypt key = (\x -> zipWith (\x y -> chr (65 + ((ord x) + (ord y)) `mod` 26)) (circularKey (cleanInput key) (length x)) (cleanInput x))

decrypt :: String -> (String -> String)
decrypt key = (\x -> zipWith (\x y -> chr (65 + ((ord y) - (ord x)) `mod` 26)) (circularKey (cleanInput key) (length x)) (cleanInput x))

empty :: String -> (String -> String)
empty _ = (\x -> x)

-- Exercise A2
frequency :: Int -> String -> [[(Char,Int)]]
frequency n ct = [tupleSwap (concat (reverse (groupBy (\a b -> fst a == fst b) (sort (tupleSwap [(c, foundTimes c lst) | c <- (rmdups lst)]))))) | lst <- (generateLists n ct)]

generateLists :: Int -> String -> [[Char]]
generateLists n ct = [[ct !! (c+i*n) | i <- [0..((length ct) `div` n)], c+i*n < length ct] | c <- [0..n-1]]

foundTimes :: Ord a => a -> [a] -> Int
foundTimes x xs = (length . filter (== x)) xs

tupleSwap :: [(a, b)]->[(b, a)]
tupleSwap = map swap

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- Exercise A3
renderMaze :: [((Integer, Integer), (Integer, Integer))] -> [String]
renderMaze [] = []
renderMaze maze = [generateLine line (getMazeSizeX maze) | line <- generateXReplacement (generatePointsYX maze) (getMazeSizeX maze) (getMazeSizeY maze)]

generatePointsYX :: [((Integer,Integer), (Integer,Integer))] -> [(Integer,Integer)]
generatePointsYX ppairs = sort $ tupleSwap (concat [generateRange (fst ppair) (snd ppair) | ppair <- ppairs ])

generateRange :: (Integer,Integer) -> (Integer,Integer) -> [(Integer,Integer)]
generateRange p1 p2 = if (fst p1) == (fst p2)
    then [(fst p1, y) | y <- [(snd p1)..(snd p2)]]
    else [(y, snd p1) | y <- [(fst p1)..(fst p2)]]

generateXReplacement :: [(Integer,Integer)] -> Integer -> Integer -> [[Integer]]
generateXReplacement yxpoints sizeX sizeY = [rmdups [snd p | p <- lst] | lst <- [[(y, x) | x <- [0..sizeX], elem (y, x) yxpoints] | y <- [0..sizeY]]]

generateLine :: [Integer] -> Integer -> String
generateLine l n = [ if elem x l then '#' else ' ' | x <- [0..n]]

getMazeSizeX :: [((Integer,Integer), (Integer,Integer))] -> Integer
getMazeSizeX maze = maximum (snd (unzip (generatePointsYX maze)))

getMazeSizeY :: [((Integer,Integer), (Integer,Integer))] -> Integer
getMazeSizeY maze = maximum (fst (unzip (generatePointsYX maze)))

tupleToList :: [(a,a)] -> [a]
tupleToList ((a,b):xs) = a : b : tupleToList xs
tupleToList _ = []