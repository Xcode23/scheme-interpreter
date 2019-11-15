module Main where

import System.Environment
import SchemeParser
import Evaluator
import CommonTypes
import Control.Monad

main :: IO ()
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (head args) >>= eval
        putStrLn $ extractValue $ trapError evaled
