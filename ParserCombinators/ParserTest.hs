module ParserTest () where

import Data.Char (digitToInt)
import Text.Parsec as P
import Text.Parsec.Char (char,digit)
import Text.Parsec.String (Parser)

data DoubleVal = TwoNum (Int,Int) | AlphaNum (Char,Int) deriving (Show)

parserDoubleVal = twoNums <|> alphaAndNum

twoNums :: Parser DoubleVal
twoNums = do
          x <- digit
          _ <- char '-'
          y <- digit
          return (TwoNum (digitToInt x, digitToInt y))

alphaAndNum :: Parser DoubleVal
alphaAndNum = do
             x <- letter
             _ <- char '-'
             y <- digit
             return (AlphaNum (x, digitToInt y))
