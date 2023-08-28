import BitGridGrammar
import BitGridTokens
import BitGridTypeChecker
import BitGridEval

import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do (fileName : _ ) <- getArgs
           sourceText <- readFile fileName
           let parsedProg = parseCalc (alexScanTokens sourceText)
           --putStrLn ("Type Checking : " ++ show parsedProg ++ "\n")
           let typedProg = typeOfProgram [] parsedProg
           --putStrLn ("Type Checking Passed with type " ++ showType typedProg ++ "\n")
           let result = evalProgram [] parsedProg
           putStrLn (showResult result)
           --writeFile "out1.tl" (showResult result)

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e ++ "\n"
               hPutStr stderr err
               return ()