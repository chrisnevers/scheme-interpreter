module Parse where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Ast

symbol :: Parser Char
symbol = oneOf "!+$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany space

escapedChar :: Parser String
escapedChar = do
  bs <- char '\\'
  c  <- oneOf "\\befnrtv\""
  return [bs, c]

nonEscapedChar :: Parser Char
nonEscapedChar = noneOf "\\\b\f\n\r\t\v\""

parseCharacters :: Parser String
parseCharacters = fmap return nonEscapedChar <|> escapedChar

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many parseCharacters
  char '"'
  return $ String $ concat str

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseVector :: Parser LispVal
parseVector = do
  char '#'
  char '('
  List x <- parseList
  char ')'
  return $ List $ Atom "vector":x

parseParens :: Parser LispVal
parseParens = do
  char '('
  x <- try parseList <|> parseDottedList
  char ')'
  return x

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> parseVector
        <|> parseParens

readExpr :: String -> String
readExpr input =
  case parse (spaces >> parseExpr) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val
