module Evaluator (readExpr, eval) where

import SchemeParser

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Complex _) = val
eval val@(Ratio _) = val
eval val@(Character _) = val
eval val@(Float _) = val
eval val@(Vector _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
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

numericBinop :: (Integer ->  Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) = let parsed = reads s :: [(Integer, String)] 
                          in if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

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
