module Main where

import System.Environment
import Control.Monad
import Ast
import Parse
import Eval

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
