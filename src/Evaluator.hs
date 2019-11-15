module Evaluator (eval) where

import CommonTypes
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Complex _) = return val
eval val@(Ratio _) = return val
eval val@(Character _) = return val
eval val@(Float _) = return val
eval val@(Vector _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?" , unaryOp symbolp) ,
              ("string?" , unaryOp stringp) ,
              ("number?" , unaryOp numberp) ,
              ("bool?", unaryOp boolp) ,
              ("list?" , unaryOp listp)]

numericBinop :: (Integer ->  Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []              = throwError $ NumArgs 2 []
numericBinop op singleVal@[_]   = throwError $ NumArgs 2 singleVal 
numericBinop op params          = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = let parsed = reads s
                        in if null parsed
                            then throwError $ TypeMismatch "number" $ String s
                            else return . fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum 

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f args = if length args /= 1 
                    then throwError $ NumArgs 1 args
                    else return . f . head $ args

symbolp (Atom _) = Bool True
symbolp _ = Bool False 
numberp (Number _) = Bool True 
numberp _ = Bool False
stringp (String _) = Bool True
stringp _ = Bool False
boolp (Bool _) = Bool True
boolp _ = Bool False
listp (List _) = Bool True
listp (DottedList _ _) = Bool False
listp _ = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""
