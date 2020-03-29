module ParserTest () where

import Data.Char (digitToInt)
import Text.Parsec as P
import Text.Parsec.Char (char,digit)
import Text.Parsec.String (Parser)

data DoubleVal = TwoNum (Int,Int) | AlphaNum (Char,Int) deriving (Show)

parserDoubleVal = twoNums <|> alphaAndNum

digitParse :: Parser Int
digitParse = digitToInt <$> digit

pairNums :: Parser (Int,Int)
pairNums = (,) <$> (digitParse <* (char '-')) <*> digitParse

twoNums :: Parser DoubleVal
twoNums = pairNums >>= (\x -> return (TwoNum x))

alphaAndNum :: Parser DoubleVal
alphaAndNum = do
             x <- letter
             _ <- char '-'
             y <- digit
             return (AlphaNum (x, digitToInt y))

data Plate = Plate Int Int Int Int Char deriving (Show)

plateNum :: Parser String
plateNum = (manyTill digit (char '-'))

plate :: Parser Plate
plate = do
        [x1,x2,x3,x4] <- count 4 plateNum
        c <- letter
        return (Plate (read x1) (read x2) (read x3) (read x4) c)
