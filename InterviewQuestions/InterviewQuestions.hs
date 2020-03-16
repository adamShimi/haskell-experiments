{-# LANGUAGE ExistentialQuantification #-}

module InterviewQuestions ( sumInt,
                            revList,
                            filterList) where

-- Questions from https://gist.github.com/pchiusano/bf06bd751395e1a6d09794b38f093787

-- Question 1 : sum up a [Int] using explicit recursion

sumInt :: [Int] -> Int
sumInt [] = 0
sumInt (x:xs) = x + sumInt xs

-- Question 2 : reverse list with fold

revList :: [a] -> [a]
revList = foldl (flip (:)) []

-- Question 3 : Implement filter for lists

filterList :: (a -> Bool) -> [a] -> [a]
filterList f = foldr (\x acc -> if f x then x:acc else acc) []

-- Question 4 : Implement filter (done above) and takeWhile with foldr

takeWhileList :: (a -> Bool) -> [a] -> [a]
takeWhileList f = foldr (helperTakeWhile f) []

helperTakeWhile :: (a -> Bool) -> a -> [a] -> [a]
helperTakeWhile f x acc
  | f x = x:acc
  | not (f x) = []

-- Question 5 : implement functions with the following types.

f1 :: forall a . a -> a
f1 = id

f2 :: forall a . a -> (a, a)
f2 x = (x,x)

f3 :: forall a b . (a -> b) -> a -> b
f3 = ($)

f4 :: forall a b c . (a -> b -> c) -> (a,b) -> c
f4 f (x,y) = f x y

-- Question 6  : implement Monad instance for Maybe

-- Rust rpz!!!
data Option a = None | Some a

instance Functor Option where
  fmap f (Some x) = Some (f x)
  fmap _ None = None

instance Applicative Option where
  pure = Some
  (Some f) <*> a = fmap f a
  None <*> _ = None

instance Monad Option where
  return = Some
  (Some x) >>= f = f x
  None >>= _ = None

lift2Mbis :: (a -> b -> c) -> Option a -> Option b -> Option c
lift2Mbis f m1 m2 = pure f <*> m1 <*> m2
