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

-- No question 6 in the web site, error of counting

-- Question 7  : implement Monad instance for Maybe

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

-- Question 8 : implement Monad instance of ((->) e)

-- instance Monad ((->) e) where
--   return = const
--   f >>= g  = \x -> (g . f) x x

-- Question 9 : implement a function of type : Applicative f => [(f a, b)] -> f [(a, b)]

onAppl :: Applicative f => [(f a, b)] -> f [(a, b)]
onAppl = traverse (\(fx,y) -> fmap (\x -> (x,y)) fx)

-- Question 10 : implement a function of type :
-- (Traversable t, Applicative f) => t (f a, b) -> f (t (a, b))
-- Same answer as previous question.

-- Question 11 : Give an Either-like data type whose Applicative accumulates errors.
-- (knowledge of functional structures, reasoning about types)

data Acc e a = LeftAcc [e] | RightAcc a

instance Functor (Acc e) where
  fmap _ (LeftAcc le) = LeftAcc le
  fmap f (RightAcc x) = RightAcc (f x)

instance Applicative (Acc e) where
  pure = RightAcc
  (RightAcc f) <*> a = fmap f a
  (LeftAcc le) <*> (RightAcc x) = LeftAcc le
  (LeftAcc le1) <*> (LeftAcc le2) = LeftAcc (le1++le2)

-- Question 12 : Does data Pair a = Pair a a have a reasonable Applicative instance?
-- If so, what is it. What about Monad? (knowledge of functional structures)

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair fx fy) <*> (Pair x y) = Pair (fx x) (fy y)

instance Monad Pair where
  return = pure
  (Pair x y) >>= f = let Pair x' _ = f x
                         Pair _ y'' = f y
                     in Pair x' y''
