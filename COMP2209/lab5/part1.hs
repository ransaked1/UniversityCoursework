--Define a function that determines whether a given element occurs within the given tree.
--You may assume that the tree is a binary search tree.

--Tree data type declaration and initialization
data Tree a = Leaf a | Node (Tree a) a (Tree a)
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord b => b -> Tree b -> Bool
occurs x (Leaf y) = compare x y == EQ
occurs x (Node l y r) = (compare x y) == EQ || (occurs y l) || (occurs y r)

main = do
    putStrLn $ "occurs: " ++ show (occurs 3 t)