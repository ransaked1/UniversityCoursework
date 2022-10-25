factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

perfect :: Int -> [Int]
perfect n = [ x | x <- [1..n], (sum (factors x)) - x == x]

main = do
    putStrLn $ "Perfects: " ++ show (perfect 500)