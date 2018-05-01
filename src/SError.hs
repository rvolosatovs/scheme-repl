module SError (LispError(..), ThrowsError, showError, trapError, extractValue) where

import SParser (LispVal, showVal, unwordsList)

import Control.Monad.Error
import Text.ParserCombinators.Parsec
             
-- Define possible ways the interpreter might fail and define how to print these errors (also making "LispError" and instance of "Show").

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

-- Our next step is to make our error type into an instance of "Error". This is necessary for it to work with GHC's built-in error handling functions.

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

-- Then we define a type to represent functions that may throw a "LispError" or return a value.
-- We only partially apply "Either" to "LispError", creating a type constructor "ThrowsError" that we can use on any data type.
     
type ThrowsError = Either LispError

-- The functions "trapError" and "extractValue" will be used in conjunction to extract the String representation of the errors, like so for example: (putStrLn $ extractValue $ trapError)

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val