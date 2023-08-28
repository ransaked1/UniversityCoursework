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
           let typedProg = typeOfProgram [] parsedProg
           let result = evalProgram [] parsedProg
           putStrLn (showResult result)

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e ++ "\n"
               hPutStr stderr err
               return ()