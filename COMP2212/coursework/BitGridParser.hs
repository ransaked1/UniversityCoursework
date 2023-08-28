import BitGridGrammar
import BitGridTokens
import System.Environment

main = do
  args <- getArgs
  s <- readFile (args !! 0)
  print (parseCalc (alexScanTokens s))