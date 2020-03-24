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

-- Problem 57 : Build binary search tree from list of values.

buildSearch :: (Ord a) => [a] -> Tree a
buildSearch [] = Empty
buildSearch (x:xs) = insert x (buildSearch xs)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Branch x Empty Empty
insert x (Branch y l r) = if x <= y
                          then Branch y (insert x l) r
                          else Branch y l (insert x r)

-- Problem 58 : Generate all symmetric completely balanced search trees.

symTrees :: Int -> [Tree Char]
symTrees = (filter isSym) . complBalTree

-- Problem 59 : Construct all height-balanced binary tree of a given maximum height

allHeightBall :: Int -> a -> [Tree a]
allHeightBall n = (filter (isHeightBal)) . (allBinary n)

-- Need to remove duplicates.
allBinary :: Int -> a -> [Tree a]
allBinary 0 _ = [Empty]
allBinary n e =
  let smaller = allBinary (n-1) e in
  ([(x,y)| x <- smaller, y <- smaller] >>= (\(x,y) -> [Branch e x y]))
  ++ smaller

isHeightBal :: Tree a -> Bool
isHeightBal Empty = True
isHeightBal (Branch _ l r) = (height l - height r <= 1) && isHeightBal l && isHeightBal r

height :: Tree a -> Int
height Empty = 0
height (Branch _ l r) = 1 + height l + height r


