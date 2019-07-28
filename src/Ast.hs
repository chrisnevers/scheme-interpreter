module Ast where

data LispVal =  Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String content) = "\"" ++ content ++ "\""
showVal (Number n) = show n
showVal (Atom id) = id
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List es) = "(" ++ unwordsList es ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"

instance Show LispVal where show = showVal
