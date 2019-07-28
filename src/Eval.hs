{-# LANGUAGE ExistentialQuantification #-}
module Eval where

import Control.Monad
import Control.Monad.Error.Class
import Ast

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val

eval (List [Atom "if", cnd, thn, els]) = do
  result <- eval cnd
  case result of
    Bool False -> eval els
    otherwise  -> eval thn

eval (List (Atom "cond":branches)) = evalCond branches
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval (List es) = mapM eval es >>= return . List

eval (DottedList es tl) = do
  args <- mapM eval es
  lst <- eval tl
  return $ DottedList args lst

-- eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalCond :: [LispVal] -> ThrowsError LispVal
evalCond [] = throwError $ Default "No matching cond case"
evalCond (List [Atom "else", body]:_) = eval body
evalCond (List [test, body]:rst) = do
  result <- eval test
  case result of
    Bool False -> evalCond rst
    otherwise  -> eval body
evalCond [body] = eval body

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" func)
  ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
  ("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  ("symbol?", checkIf isSymbol),
  ("string?", checkIf isString),
  ("number?", checkIf isNumber),
  ("=", numBoolBinop (==)),
  ("<", numBoolBinop (<)),
  (">", numBoolBinop (>)),
  ("<=", numBoolBinop (<=)),
  (">=", numBoolBinop (>=)),
  ("&&", boolBoolBinop (&&)),
  ("||", boolBoolBinop (||)),
  ("string=?", strBoolBinop (==)),
  ("string<?", strBoolBinop (<)),
  ("string>?", strBoolBinop (>)),
  ("string<=?", strBoolBinop (<=)),
  ("string>=?", strBoolBinop (>=)),
  ("car", car),
  ("cdr", cdr),
  ("cons", cons),
  ("eq?", eqv),
  ("eqv?", eqv),
  ("equal?", equal)]

car :: [LispVal] -> ThrowsError LispVal
car [List (hd:_)]         = return hd
car [DottedList (hd:_) _] = return hd
car [arg]                 = throwError $ TypeMismatch "pair" arg
car badArgs               = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgs  = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs lst] = return $ DottedList (x:xs) lst
cons [x1, x2]     = return $ DottedList [x1] x2
cons badArgs      = throwError $ NumArgs 2 badArgs

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Atom a1, Atom a2] = return $ Bool $ a1 == a2
eqv [Bool a1, Bool a2] = return $ Bool $ a1 == a2
eqv [Number a1, Number a2] = return $ Bool $ a1 == a2
eqv [String a1, String a2] = return $ Bool $ a1 == a2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List xs, List ys] = return $ Bool $  (length xs == length ys) &&
                                          (all eqvPair $ zip xs ys)
                  where eqvPair (x1, x2) = case eqv [x1,x2] of
                                          Left err -> False
                                          Right (Bool val) -> val
eqv [_, _]  = return $ Bool False
eqv badArgs = throwError $ NumArgs 2 badArgs

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
  then throwError $ NumArgs 2 args
  else do left <- unpacker $ head args
          right <- unpacker $ args !! 1
          return $ Bool $ left `op` right

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []          = throwError $ NumArgs 2 []
numericBinop op single@[_]  = throwError $ NumArgs 2 single
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                        if null parsed
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notStr     = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b)  = return b
unpackBool notBool   = throwError $ TypeMismatch "bool" notBool

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol (List (Atom "quote":_)) = True
isSymbol _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _ = False

checkIf :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
checkIf fn es = return $ Bool $ all fn es

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a1 a2 (AnyUnpacker unpacker) =
  do  unpacked1 <- unpacker a1
      unpacked2 <- unpacker a2
      return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [a1, a2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals a1 a2)
          [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [a1, a2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgs = throwError $ NumArgs 2 badArgs
