--Construct the same graph as in part 1 but this time use cyclic dependencies.
import Data.Graph
import Data.List

--Utility function
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

graph :: Graph
graph = buildG (0, 1000) edges
    where edges = rmdups ([(x, x+1) | x <- [0..998], even x] ++ [(x, x `div` 5) | x <- [0..1000], odd x] ++ [(x+1, x) | x <- [0..998], even x] ++ [(x `div` 5, x) | x <- [0..1000], odd x])

--Write a function isReachable that decides whether there is a path between two given nodes in your graph defined above.
isReachable :: Int -> Int -> Bool
isReachable x y = path graph x y

main = do
  print $ "The edges are " ++ (show.edges) graph
  print $ "The vertices are " ++ (show.vertices) graph
  print $ "Is reachable 2 0: " ++ show (isReachable 2 0)
  print $ "Is reachable 1 2: " ++ show (isReachable 1 2)