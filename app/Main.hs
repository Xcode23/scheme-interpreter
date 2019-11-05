module Main where

import System.Environment
import SchemeParser

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn ("Hello, " ++ readExpr expr)
