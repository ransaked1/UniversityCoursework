-- using head and tail
fourth1 :: [a] -> a
fourth1 a = head (tail ( tail ( tail (a))))

-- using !!
fourth2 :: [a] -> a
fourth2 a = a !! 3

-- using pattern matching
fourth3 :: [a] -> a
fourth3 (_:_:_:a:_) = a

main = do
    putStrLn $ "fourth [1,2,3,4,5,6] : " ++ show (fourth1 [1,2,3,4,5,6])
    putStrLn $ "fourth [1,2,3,4,5,6] : " ++ show (fourth2 [1,2,3,4,5,6])
    putStrLn $ "fourth [1,2,3,4,5,6] : " ++ show (fourth3 [1,2,3,4,5,6])
