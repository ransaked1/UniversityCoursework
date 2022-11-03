{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2022

--EXERCISE A2 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

--module Exercises (frequency) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Extra imports needed
import Data.List
import Data.Tuple

-- Exercise A2
frequency :: Int -> String -> [[(Char,Int)]]
--frequency _ _ = [[(' ',0)]]
frequency n ct = [tupleSwap (concat (reverse (groupBy (\a b -> fst a == fst b) (sort (tupleSwap [(c, foundTimes c lst) | c <- (rmdups lst)]))))) | lst <- (generateLists n ct)]

generateLists :: Int -> String -> [[Char]]
generateLists n ct = [[ct !! (c+i*n) | i <- [0..((length ct) `div` n)], c+i*n < length ct] | c <- [0..n-1]]

foundTimes :: Ord a => a -> [a] -> Int
foundTimes x xs = (length . filter (== x)) xs

tupleSwap :: [(a, b)]->[(b, a)]
tupleSwap = map swap

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

main = do
    --putStrLn $ "generate: " ++ show (generateLists 3 "jihgfedcba")
    --putStrLn $ "sort: " ++ show (sortLists 3 (generateLists 3 "jihgfedcba"))
    --putStrLn $ "times found: " ++ show (foundTimes 'a' "banana")
    putStrLn $ "frequency: " ++ show (frequency 1 "SQUEAMISHOSSIFRAGE")
    putStrLn $ "frequency: " ++ show (frequency 4 "SQUEAMISHOSSIFRAGE")
    putStrLn $ "frequency: " ++ show (frequency 2 "BBAAAABB")
    --putStrLn $ "swap: " ++ show (tupleSwap [('A',2),('E',2),('F',1),('G',1),('H',1),('I',2),('M',1),('O',1),('Q',1),('R',1),('S',4),('U',1)])
