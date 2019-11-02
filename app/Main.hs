module Main where
import System.Environment
import Lib

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn ("Hello, " ++ readExpr expr)
