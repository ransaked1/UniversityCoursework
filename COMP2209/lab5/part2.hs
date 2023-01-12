--Write a function that implements a fold function for the Tree data type. Use your foldTree function to implement
--a function that creates a list of values stored in the given tree.

--Tree data type declaration and initialization
data Tree a = Leaf a | Node (Tree a) a (Tree a)
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

foldTree :: (a -> b) -> (b -> a -> b -> b) -> Tree a -> b
foldTree f g (Leaf y) = f y
foldTree f g (Node l y r) = g (foldTree f g l) y (foldTree f g r)

flatten :: Tree a -> [a]
flatten t = foldTree (\x -> [x]) (\x y z -> [y] ++ x ++ z) t

main = do
    putStrLn $ "foldTree: " ++ show (foldTree (*2) (\x y z -> x + y + z) t)
    putStrLn $ "flattenTree: " ++ show (flatten t)