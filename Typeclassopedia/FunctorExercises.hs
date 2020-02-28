module FunctorExercises
       (EitherBis)
       where

data EitherBis a b = LeftBis a | RightBis b

instance Functor (EitherBis e) where
  fmap _ (LeftBis err) = LeftBis err
  fmap f (RightBis val) = RightBis (f val)
