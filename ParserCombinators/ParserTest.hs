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
  >>= (return . TwoNum)

alphaAndNum :: Parser DoubleVal
alphaAndNum =
  (,) <$> letter <* char '-' <*> digitParse
  >>= (return . AlphaNum)

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


-- Basic expression grammar (with parentheses and no spaces)

data Expr =
  Val Int
  | Op Bop Expr Expr
  deriving (Eq, Show)

data Bop = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Show)

parseVal :: Parser Expr
parseVal = (many1 digit) >>= (return . Val . read)

parseBopSign :: Parser Bop
parseBopSign = char '+' *> return Plus
              <|> char '-' *> return Minus
              <|> char '*' *> return Times
              <|> char '/' *> return Divide

parseBop :: Parser Expr
parseBop = (,,) <$> (char '(' *> parseExpr)
                <*> (spaces *> parseBopSign <* spaces)
                <*> (parseExpr <* char ')')
            >>= (\(x,bop,y) -> return $ Op bop x y)

parseExpr :: Parser Expr
parseExpr = spaces *> choice [parseVal,parseBop] <* spaces
