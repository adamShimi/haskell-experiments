module FunctorExercises
       (Either)
       where

import Prelude hiding (Either, Left, Right)

-- Instances

-- Exercise 1

data Either a b = Left a | Right b

instance Functor (Either e) where
  fmap _ (Left err) = Left err
  fmap f (Right val) = Right (f val)

-- Will not compile
-- instance ((->) e) where
--   fmap = (.)

-- Exercise 2

-- Tuple (my version of (,)) can have two elements of different types,
-- whereas Pair has two elements of the same type.

data Tuple a b = Tuple a b

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a (f b)

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

-- Exercise 3

data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor ITree where
  fmap f (Leaf l) = Leaf (f . l)
  fmap f (Node xs) = Node (map (fmap f) xs)
