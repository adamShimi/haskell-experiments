module SaddlebackSearch () where

-- Function invert that takes a function f : (Int,Int) -> Int, from naturals to
-- naturals, a natural z,
-- and return the set of pairs (x,y) such that f (x,y) = z
-- We know that f is increasing in each argument.

-- Straightforward answer

invert :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
invert f z = [(x,y) | x <- [0..z], y <- [0..z], f (x,y) == z]

-- Apparently, stricly increasing means that f(x,y) >= x+y

invert1 f z = [(x,y) | x <- [0..z], y <- [0..z-x], f (x,y) == z]


