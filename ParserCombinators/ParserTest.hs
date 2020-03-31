module ParserTest () where

import Data.Char (digitToInt)
import Text.Parsec as P
import Text.Parsec.Char (char,digit)
import Text.Parsec.String (Parser)

data DoubleVal = TwoNum (Int,Int) | AlphaNum (Char,Int) deriving (Show)

parserDoubleVal = twoNums <|> alphaAndNum

twoNums :: Parser DoubleVal
twoNums =
  (,) <$> digitParse <* (char '-') <*> digitParse
  >>= (\x -> return (TwoNum x))

alphaAndNum :: Parser DoubleVal
alphaAndNum =
  (,) <$> letter <* char '-' <*> digitParse
  >>= (\(x,y) -> return (AlphaNum (x,y)))

digitParse :: Parser Int
digitParse = digitToInt <$> digit

data Plate = Plate Int Int Int Int Char deriving (Show)

plate :: Parser Plate
plate =
  (,) <$> count 4 plateNum  <*> letter
  >>= (\([x1,x2,x3,x4],c) -> return (Plate (read x1) (read x2) (read x3) (read x4) c))

plateNum :: Parser String
plateNum = (manyTill digit (char '-'))

wordsBis :: Parser [String]
wordsBis = spaces *> many (many1 (noneOf [' ']) <* spaces) <* eof
