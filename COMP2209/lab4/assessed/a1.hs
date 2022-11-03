{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2022

--EXERCISE A1 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

--module Exercises (vigenere) where

-- The following two imports are needed for testing, do not delete

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Extra imports needed
import Data.Char

-- Exercise A1
vigenere :: String -> (String -> String, String -> String)
--vigenere _ = ( \_ -> "FAIL" , \_ -> "FAIL")
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

main = do
    --putStrLn $ "clean ' 123ban -+-ana456 ': " ++ show (cleanInput " 123bAN -+-aNa456 ")
    --putStrLn $ "circularKey 'AYUSH': " ++ show (circularKey (cleanInput " 123aYu -+-Sh456 ") (length "GEEKSFORGEEKS"))
    --putStrLn $ "zip 'AYUSHAYUSHAYU' 'GCYCZFMLYLEIM': " ++ show (decrypt (cleanInput " 123aYu -+-Sh456 ") (cleanInput "GCYCZFMLYLEIM"))
    putStrLn $ "(fst (vigenere 'SQUEAMISH')) 'OSSIFRAGE' :" ++ show ((fst (vigenere "SQUEAM")) "GIMMFDSWY")
    --putStrLn $ "(fst (vigenere 'SQUEAMISH')) 'OSSIFRAGE' :" ++ show ((fst (vigenere "SQUEAMISH")) "OSSIFRAGE")