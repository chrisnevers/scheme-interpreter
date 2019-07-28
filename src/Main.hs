module Main where

import System.Environment
import Parse

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ "Input: " ++ expr
  putStrLn $ readExpr expr

