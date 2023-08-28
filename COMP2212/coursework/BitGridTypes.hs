import BitGridGrammar
import BitGridTokens
import BitGridTypeChecker
import System.Environment

main = do
  args <- getArgs
  s <- readFile (args !! 0)
  print (showType (typeOfProgram [] (parseCalc (alexScanTokens s))))