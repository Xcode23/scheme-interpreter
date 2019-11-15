module CommonTypes 
  (LispVal(..), 
  LispError(..),
  ThrowsError,
  trapError,
  extractValue) where

import qualified Data.Vector as V
import Data.Ratio
import Data.Complex
import Control.Monad.Except
import Text.ParserCombinators.Parsec

data LispVal  = Atom String
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

showVal :: LispVal -> String
showVal (String string) = "\"" ++ string ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool value) = if value then "#t" else "#false"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector vector) = "#(" ++ (unwordsList $ V.toList vector) ++ ")"
showVal (Float value) = show value 
showVal (Ratio value) = show (numerator value) ++ "/" ++ show (denominator value)
showVal (Complex value) = show (realPart value) ++ "+" ++ show (imagPart value) ++ "i"
showVal (Character char) | char == '\n' = "#\\newline"
                         | char == ' '  = "#\\space"
                         | otherwise    = "#\\" ++ [char]

unwordsList = unwords . map showVal


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String 
               | UnboundVar String String 
               | Default String

instance Show LispError where show = showError

type ThrowsError = Either LispError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found 
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found 
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val