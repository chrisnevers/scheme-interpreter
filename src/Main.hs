module Main where

import System.Environment
import Parse
import Eval

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
