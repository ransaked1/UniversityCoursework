import Tokens
import System.Environment

--Uncomment the code for the part to test
main = do
  --Part1
  args <- getArgs
  s <- readFile (args !! 0)
  print (alexScanTokens s)