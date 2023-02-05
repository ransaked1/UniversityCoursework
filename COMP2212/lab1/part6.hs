import System.IO
import Data.List.Split
import qualified Data.ByteString.Lazy as BSL
import Data.List
import System.Environment
import Control.Exception
import Data.Typeable
import Control.Exception.Base

takeFst :: [a] -> [a]
takeFst [] = []
takeFst (x:xs) = [x]

takeRest :: [a] -> [a]
takeRest [] = []
takeRest (x:xs) = xs

checkAllEmpty :: (Integral a, Eq a) => [[a]] -> Bool
checkAllEmpty lsts = not (any (True==) (map (\lst -> lst /= []) lsts))

multiZipL :: (Integral a, Show a) => [[a]] -> [[a]]
multiZipL lsts | checkAllEmpty lsts = []
               | otherwise = (concat $ map (\lst -> takeFst lst) lsts) : multiZipL (map (\lst -> takeRest lst) lsts)

strsToL :: [String] -> [[Int]]
strsToL strs = [map (read::String->Int) (filter (not . null) ((reverse . tail . reverse) $ splitOn "," str)) | str <- strs]

lToStrs :: [[Int]] -> [String]
lToStrs lsts = [ intercalate "," (map show lst) ++ "\r" | lst <- lsts]

multiZipF :: String -> String -> IO ()
multiZipF input output = do
    i <- openFile input ReadMode
    cont <- hGetContents i
    let lsts = strsToL (lines cont)
    writeFile output (concat (lToStrs (multiZipL lsts)))

main = do
    args <- getArgs
    catch (multiZipF (args !! 0) (args !! 1)) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Exception: " ++ show ex