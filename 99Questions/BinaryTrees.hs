-- Problems 55 to 60

module BinaryTrees (Tree) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)


-- Problem 55 : Construct completely balanced binary trees.

complBalTree :: Int -> [Tree Char]
complBalTree 0 = [Empty]
complBalTree 1 = [Branch 'x' Empty Empty]
complBalTree n =
  (part2 n) >>= (\(nbl,nbr) -> [Branch 'x' l r | l <- (complBalTree nbl), r <- (complBalTree nbr)])

part2 :: Int -> [(Int,Int)]
part2 n
  | n == 0 = [(0,0)]
  | even n = let n2 = n `div` 2 in [(n2,n2)]
  | odd n = let n2 = n `div` 2 in [(n2, n2+1),(n2+1,n2)]

-- Problem 56 : Check that a tree is symmetric

isSym :: Tree a -> Bool
isSym Empty = True
isSym (Branch _ l r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Branch _ l1 r1) (Branch _ l2 r2) = (mirror l1 r2) && (mirror r1 l2)
