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


-- Basic expression grammar

data Expression =
  Val Int
  | Op Bop Int Int
  deriving (Eq, Show)

data Bop = Plus
  | Minus
--  | Times
--  | Divide
  deriving (Eq, Show)

parseVal :: Parser Expression
parseVal = (many1 digit <* spaces <* eof) >>= (\x -> return $ Val (read x))

parseBop :: Parser Expression
parseBop = do
           x <- many1 digit
           bop <- spaces *> (char '+' <|> char '-') <* spaces
           y <- many1 digit
           return $ if bop == '+' then Op Plus (read x) (read y)
                                  else Op Minus (read x) (read y)

parseExpr :: Parser Expression
parseExpr = spaces *> ( try parseVal <|> parseBop)
