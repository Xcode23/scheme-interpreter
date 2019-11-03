module Lib
  (readExpr)
    where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment 
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany space

escapedCharacters :: Parser Char
escapedCharacters = do
                      char '\\'
                      x <- oneOf "\\\"nrt"
                      return $ case x of
                                 '\\' -> x
                                 '"'  -> x
                                 'n'  -> '\n'
                                 't'  -> '\t'
                                 'r'  -> '\r'

parseString :: Parser LispVal
parseString = do
               char '"'
               x <- many $ escapedCharacters <|> noneOf "\\\""
               char '"'
               return $ String x

parseBool :: Parser LispVal
parseBool = try $ do
              char '#'
              (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAtom :: Parser LispVal
parseAtom = do
             first <- letter <|> symbol
             rest <- many $ letter <|> digit <|> symbol
             let atom = first:rest
             return $ Atom atom

parseDecimal :: Parser LispVal
parseDecimal = Number . read <$> many1 digit

myParseDecimal :: Parser LispVal
myParseDecimal = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
                  try $ string "#d"
                  x <- many1 digit
                  (return . Number . read) x

parseOct :: Parser LispVal
parseOct = do
              try $ string "#o"
              x <- many1 octDigit
              (return . Number . oct2dig) x

parseHex :: Parser LispVal
parseHex = do
            try $ string "#x"
            x <- many1 hexDigit
            (return . Number . hex2dig) x

parseBin :: Parser LispVal
parseBin = do
            try $ string "#b"
            x <- many1 $ oneOf "10"
            (return . Number . bin2dig) x

oct2dig x = fst $ head $ readOct x

hex2dig x = fst $ head $ readHex x

bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1)
                            in bin2dig' old xs

parseNumber :: Parser LispVal
parseNumber = myParseDecimal
              <|> parseDecimal2
              <|> parseHex
              <|> parseBin
              <|> parseOct

parseExpr :: Parser LispVal
parseExpr = parseBool
            <|> parseString
            <|> parseNumber
            <|> parseAtom

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> show val

