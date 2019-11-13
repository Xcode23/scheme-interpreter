module SchemeParser 
  (readExpr, LispVal(..)) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment 
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import qualified Data.Vector as V

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (V.Vector LispVal)
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)

instance Show LispVal where show = showVal

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

parseCharacter :: Parser LispVal
parseCharacter = do
                  try $ string "#\\"
                  value <- try (string "newline" <|> string "space")
                    <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x]}
                  return $ Character $ case value of 
                                         "space"    -> ' '
                                         "newline"  -> '\n'
                                         otherwise  -> (head value)

parseFloat :: Parser LispVal
parseFloat = do
              x <- many1 digit
              char '.'
              y <- many1 digit
              return $ Float (fst.head $ readFloat (x++"."++y))

parseRatio :: Parser LispVal
parseRatio = do
              x <- many1 digit
              char '/'
              y <- many1 digit
              return $ Ratio (read x % read y)

toDouble :: LispVal -> Double
toDouble (Float d) = d
toDouble (Number n) = fromInteger n
  
parseComplex :: Parser LispVal
parseComplex = do
                x <- (try parseFloat <|> parseDecimal)
                char '+'
                y <- (try parseFloat <|> parseDecimal)
                char 'i'
                return $ Complex (toDouble x :+ toDouble y)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                   head <- endBy parseExpr spaces
                   tail <- char '.' >> spaces >> parseExpr 
                   return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
               char '\''
               x <- parseExpr 
               return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
                    char '`'
                    x <- parseExpr
                    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
                char ','
                x <- parseExpr
                return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do 
                vectorValues <- sepBy parseExpr spaces
                return $ Vector (V.fromList vectorValues)

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try parseComplex
            <|> try parseFloat
            <|> try parseRatio
            <|> try parseNumber
            <|> try parseBool
            <|> try parseCharacter
            <|> parseQuoted 
            <|> parseQuasiQuoted
            <|> parseUnQuote
            <|> try  (do
                        string "#("
                        x <- parseVector 
                        char ')'
                        return x)
            <|> do 
                  char '('
                  x <- try parseList <|> parseDottedList 
                  char ')'
                  return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> String $ "No match: " ++ show err
                   Right val -> val

showVal :: LispVal -> String
showVal (String string) = "\"" ++ string ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool value) = if value then "#t" else "#false"
showVal (List contents) = "(" ++ unwordsLispVal contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsLispVal head ++ " . " ++ showVal tail ++ ")"
showVal (Vector vector) = "#(" ++ (unwordsLispVal $ V.toList vector) ++ ")"
showVal (Float value) = show value 
showVal (Ratio value) = show (numerator value) ++ "/" ++ show (denominator value)
showVal (Complex value) = show (realPart value) ++ "+" ++ show (imagPart value) ++ "i"
showVal (Character char) | char == '\n' = "#\\newline"
                         | char == ' '  = "#\\space"
                         | otherwise    = "#\\" ++ [char]

unwordsLispVal = unwords . map showVal
