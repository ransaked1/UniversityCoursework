--Use the Data.Graph module to build a graph as follows:
--Nodes are numbered as Ints from 0 to 1000
--There is an edge from every even numbered node (<1000) N to node (N+1)
--There is an edge from every odd numbered node N to the node (N div 5)

import Data.Graph

graph :: Graph
graph = buildG (0, 1000) edges
    where edges = [(x, x+1) | x <- [0..998], even x] ++ [(x, x `div` 5) | x <- [0..1000], odd x]

--Write a function isReachable that decides whether there is a path between two given nodes in your graph defined above.
isReachable :: Int -> Int -> Bool
isReachable x y = path graph x y

main = do
  print $ "The edges are " ++ (show.edges) graph
  print $ "The vertices are " ++ (show.vertices) graph
  print $ "Is reachable 2 0: " ++ show (isReachable 2 0)
  print $ "Is reachable 1 2: " ++ show (isReachable 1 2)