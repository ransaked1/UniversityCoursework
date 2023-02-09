import Tokens
import System.Environment

--Uncomment the code for the part to test
main = do
  --Part1
  --args <- getArgs
  --s <- readFile (args !! 0)
  --print (alexScanTokens s)

  --Part2
  --args <- getArgs
  --s <- readFile (args !! 0)
  --print (alexScanTokens s)
  --print (tokenPosn (alexScanTokens s !! 0))
  --print (tokenPosn (alexScanTokens s !! 1))
  --print (tokenPosn (alexScanTokens s !! 2))
  --print (tokenPosn (alexScanTokens s !! 5))
  --print (tokenPosn (alexScanTokens s !! 8))
  --print (tokenPosn (alexScanTokens s !! 11))

  --Part3
  args <- getArgs
  s <- readFile (args !! 0)
  print (alexScanTokens s)
  print (tokenPosn (alexScanTokens s !! 0))
  print (tokenPosn (alexScanTokens s !! 1))
  print (tokenPosn (alexScanTokens s !! 2))
  print (tokenPosn (alexScanTokens s !! 5))
  print (tokenPosn (alexScanTokens s !! 8))
  print (tokenPosn (alexScanTokens s !! 11))
  print (tokenPosn (alexScanTokens s !! 15))
  print (tokenPosn (alexScanTokens s !! 20))
  print (tokenPosn (alexScanTokens s !! 25))