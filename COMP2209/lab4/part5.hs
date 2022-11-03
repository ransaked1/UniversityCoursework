--Define a function that accepts two functions and alternately applies them to successive elements in a list.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs

main = do
    putStrLn $ "altMap (+10) (+100) [0,1,2,3,4]: " ++ show (altMap (+10) (+100) [0,1,2,3,4])