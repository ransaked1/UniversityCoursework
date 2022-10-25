import Data.Char

enc :: Int -> String -> String
enc n [] = []
enc n (x:xs) = chr (ord x + n) : enc n xs

encrypt :: Int -> String -> (String , String -> String)
encrypt n xs = (enc n xs, \x -> x)

main = do
    let (cipher,decrypt) = encrypt 5 "Banana"
    putStrLn $ "Result: " ++ show (decrypt cipher)
    putStrLn $ "Result enc: " ++ show (decrypt (encrypt 5 "Banana"))