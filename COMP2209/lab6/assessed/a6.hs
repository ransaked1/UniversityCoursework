{-# LANGUAGE DeriveGeneric #-}

--TEMPLATE FILE FOR COURSEWORK 2 for COMP2209
--Julian Rathke, Oct 2022

--EXERCISE A6 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

--module Exercises (insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where

-- Extra imports needed
import Data.List

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = Lt a Int (VTree a) | Rt a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

--Utilitarian
mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\z -> \x -> insertFromCurrentNode x z) (Leaf,[])

incrementNode :: Ord a => VTree a -> VTree a
incrementNode Leaf = Leaf
incrementNode (Node l x y r) = (Node l x (y+1) r)

goLeft,goRight,goUp :: Ord a => (VTree a, Trail a) -> (VTree a, Trail a)

goLeft (Node l x y r , ts) = (incrementNode l , Lt x y r:ts)
goRight (Node l x y r , ts) = (incrementNode r , Rt x y l:ts)

goUp (t , Lt x y r : ts) = (Node t x (y+1) r , ts)
goUp (t , Rt x y l : ts) = (Node l x (y+1) t , ts)

attach :: Ord a => VTree a -> Zipper a -> Zipper a
attach t ( _ , ts) = (t, ts)

-- Exercise A6
insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode x (Leaf, []) = attach (Node Leaf x 1 Leaf) (Leaf, [])
insertFromCurrentNode x zip = if fst (travelDown x (goRoot x zip)) == Leaf
    then attach (Node Leaf x 1 Leaf) (travelDown x (goRoot x zip))
    else travelDown x (goRoot x zip)

goRoot :: Ord a => a -> Zipper a -> Zipper a
goRoot n (t , []) = (t, [])
goRoot n (Node l1 x1 y1 r1, (Rt x2 y2 r2:ts)) =
    if x1==n
        then (Node l1 x1 y1 r1, (Rt x2 y2 r2:ts))
    else if n > x2 && n < x1
        then goUp (Node l1 x1 y1 r1, (Rt x2 y2 r2:ts))
    else goRoot n (goUp (Node l1 x1 y1 r1, (Rt x2 y2 r2:ts)))
goRoot n (Node l1 x1 y1 r1, (Lt x2 y2 r2:ts)) =
    if x1==n
        then (Node l1 x1 y1 r1, (Lt x2 y2 r2:ts))
    else if n < x2 && n > x1
        then goUp (Node l1 x1 y1 r1, (Lt x2 y2 r2:ts))
    else goRoot n (goUp (Node l1 x1 y1 r1, (Lt x2 y2 r2:ts)))

travelDown :: Ord a => a -> Zipper a -> Zipper a
travelDown n (Leaf, ts) = (Leaf, ts)
travelDown n (Node l x y r, ts) =
    if n == x
        then (Node l x y r, ts)
    else if n < x
        then travelDown n (goLeft (Node l x y r, ts))
    else travelDown n (goRight (Node l x y r, ts))

main = do
    putStrLn $ "Test1: " ++ show (mkTree [5,5])
    putStrLn $ "Test2: " ++ show (mkTree [3,2,3])
    putStrLn $ "Test3: " ++ show (mkTree [5,4,3,2,1])
    putStrLn $ "Test4: " ++ show (mkTree "hello")
    putStrLn $ "Test5: " ++ show (mkTree [6,8,3,2,9,7,5,1,4])
    putStrLn $ "Test6: " ++ show (mkTree [6,8,3,2,12,9,11,5,4,1])