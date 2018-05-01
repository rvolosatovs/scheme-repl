{-# LANGUAGE ExistentialQuantification #-}

module SEvaluator (eval) where

import SExpr (readExpr)
import SParser (LispVal(..), showVal)
import SError (LispError(..), ThrowsError)

import Control.Monad.Error

-- The purpose of an evaluator is to map some code to some data. In Lisp, the data types for both code and data are the same, so our evaluator will return a "LispVal".
-- Evaluating numbers, strings, booleans, and quoted lists is simple enough: Just return the value. The last clause in "eval" handles function application.

-- The @ operator is used to have access to both the constructor and its values. It's useful if you want to "decompose" a parameter into it's parts while still needing the parameter as a whole.
-- In this particular case, it makes sure the "eval" function fails if fed a LispVal whose treatment has not been specified. This is useful if we later want to expand the "LispVal" type.

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

-- The built-in function "lookup" looks up a key (its first argument) in a list of pairs and retuns a Maybe. 
-- We use the built-in function "maybe" so that if the function isn't found, we throw an error.
-- If the function is indeed found, we apply it to the arguments.

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)

-- We define the primitives we expect to find as functions in the Scheme language as a List of pairs, containing the key we are to find with "lookup" and the function we are to apply to the arguments.

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
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

-- By using "foldl" we are not limiting ourselves to only two arguments. Also, we're implementing a form of weak typing. That means that, if a value can be interpreted as a number, we'll use it as one.
-- We use an @-pattern once again to capture the single-value case because we want to include the actual value passed in for error-reporting purposes.
-- We also need to use "mapM" to sequence the results of "unpackNum", because each individual call to unpackNum may fail with a "TypeMismatch".

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError (NumArgs 2 [])
numericBinop op singleVal@[_] = throwError (NumArgs 2 singleVal)
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError (TypeMismatch "number" (String n))
                             else return (fst (parsed !! 0))
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError (TypeMismatch "number" notNum)

-- The "_Binop" functions that remain to be defined all take exactly two arguments and return a Boolean, they differ from each other only in the type of argument they expect. We write a generic "boolBinop" function.

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do {
                                left <- unpacker $ args !! 0;
                                right <- unpacker $ args !! 1;
                                return $ Bool $ left `op` right;
                            }
                                      
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- We are going to need a function similar to "unpackNum" in order to unpack Strings, and another one to unpack Booleans.

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return (show s)
unpackStr (Bool s) = return (show s)
unpackStr notString = throwError (TypeMismatch "string" notString)

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError (TypeMismatch "boolean" notBool)

-- We also add in the basic list-handling primitives. The function "car" returns the first element of a list, while "cdr" returns what remains of the list after removing the first element.

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError (TypeMismatch "pair" badArg)
car badArgList = throwError (NumArgs 1 badArgList)

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return (List xs)
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return (DottedList xs x)
cdr [badArg] = throwError (TypeMismatch "pair" badArg)
cdr badArgList = throwError (NumArgs 1 badArgList)

-- The "cons" function constructs lists; it is the inverse of "car" and "cdr". It's a little tricky, so we are giving explanations on each case in the function.

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return (List [x1]) -- If you "cons" together anything with "Nil", you end up with a one-item list, the "Nil" serving as a terminator.
cons [x, List xs] = return (List (x : xs)) -- If you "cons" together anything and a list, it's like tacking that anything onto the front of the list.
cons [x, DottedList xs xlast] = return (DottedList (x : xs) xlast) -- However, if the list is a "DottedList", then it should stay a "DottedList", taking into account the improper tail.
cons [x1, x2] = return (DottedList [x1] x2) -- If you "cons" together two non-lists, or put a list in front, you get a "DottedList".
cons badArgList = throwError (NumArgs 2 badArgList) -- Finally, attempting to "cons" together more or less than two arguments is an error.

-- For our purposes, Scheme functions "eq?" and "eqv?" are basically the same: they recognize two items as the same if they print the same, and are fairly slow. So we can write one function for both of them.

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return (Bool (arg1 == arg2))
eqv [(Number arg1), (Number arg2)] = return (Bool (arg1 == arg2))
eqv [(String arg1), (String arg2)] = return (Bool (arg1 == arg2))
eqv [(Atom arg1), (Atom arg2)] = return (Bool (arg1 == arg2))
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [(List arg1), (List arg2)] = return (Bool ((length arg1 == length arg2) && (all eqvPair (zip arg1 arg2))))
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _] = return (Bool False)
eqv badArgList = throwError (NumArgs 2 badArgList)

-- Since we introduced weak typing above, we'd also like to introduce an "equal" function that ignores differences in the type tags and only tests if two values can be interpreted the same.
-- Basically, we want to try all of our unpack functions, and if any of them result in Haskell values that are equal, return True.

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)
   
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList