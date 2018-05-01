module SParser (LispVal(..), showVal, unwordsList, parseExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

-- The precise rules for forming identifiers vary among implementations of Scheme, but in general an identifier has to start with an extended alphabatic character that cannot begin a number.
-- The identifier can then be followed by any number of extended alphabatic characters, that is some number, lower or upper case letter, or one of these: !#$%&|*+-/:<=>?@^_~
-- Our first parser "symbol" takes care of recognising those extended characters, while the Parsec library provides pre-built parsers such as "letter" or "digit".
         
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- We start developing what will later be our expression reader. As it is now, it simply reads a String and returns a String, telling us if it was succesfully parsed or not.
-- The "parse" function from Parsec takes a parser and a pair of Strings as inputs and then, following typical Haskell convention, returns an Either data type.
-- The first String is the name of the actual input String, which is the second one. The Either uses the Left constructor to indicate an error and the Right one for a normal value.

readExpr01 :: String -> String
readExpr01 input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
    
-- As it is, our expression reader is incapable of handling white spaces. The Parsec library includes a "space" parser that does exactly what its name implies. What we want is slightly different though.
-- Parsec introduces the concept of combinators: Functions that take a parser as an input and return a modified version of that very same parser. The "skipMany1" combinator is of particular interest for us.
-- Feeding the "space" parser to the "skipMany1" combinator, we obtain a parser that skips one or more occurrences of the space character.

spaces :: Parser ()
spaces = skipMany1 space

-- We combine our two parsers by using the bind operator, which in this context means: "Attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails."

readExpr02 :: String -> String
readExpr02 input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
    
-- Our expression reader is still useless though, as it only tells us if it was successful or not. We want our parsers to convert the input into a data structure we can manipulate easily.
-- The LispVal data type defines all possible values we can encounter in the Scheme language. We now define a parser for each of them.
-- Notice that the "String" constructor defined in "LispVal" has the same name as the type String. This is not an issue, as they have different namespaces.

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- A string is a double quote mark, followed by any number of non-quote characters, followed by a closing quote mark.

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return (String x)
                
-- An atom is a letter or symbol, followed by any number of letters, digits, or symbols.
-- The <|> operator tries the first parser, then if it fails, tries the second. If either succeeds, then it returns the value returned by that parser

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom
                         
-- Here we read any number of digits, constructing a new parser for the purpose by applying the "many1" combinator to the "digit" parser.
-- We are going to end up with a String, so we'll want to convert it to a number using the function "read", in order to then use the "Number" constructor.
-- The function LiftM promotes a function to a monad, allowing us to use "Number . read" on the wrapped value inside what is returned from "many1 digit".

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)

-- Anologously to "parseNumber", the parser "parseList" uses "LiftM" to use the constructor "List" on the wrapped value inside what is returned from "sepBy parseExpr spaces".

parseList :: Parser LispVal
parseList = liftM List (sepBy parseExpr spaces)

-- The parser "parseDottedList" works similarly to "parseList", until it finds a dot. Then it ignores all white spaces and parses the following expression.

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return (DottedList head tail)

-- Finally, we add support for the single-quote syntactic sugar of Scheme. It reads a single quote character, reads an expression and binds it, and then returns a List composed of "quote" and the result.

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return (List [Atom "quote", x])

-- We combine all of these parsers to get one that reads any "LispVal". This implementation illustrates an intereseting feature of Parsec: backtracking.
-- The parsers "parseList" and "parseDottedList" recognize identical strings up to the dot; this breaks the requirement that a choice alternative may not consume any input before failing.
-- The try combinator attempts to run the specified parser, but if it fails, it backs up to the previous state. This lets you use it in a choice alternative without interfering with the other alternative.

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

-- Integrating "parseExpr" into our expression reader we finally get something useful, making it so it returns the expression instead of a String.

readExpr03 :: String -> LispVal
readExpr03 input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val
    
-- It'll prove useful to be able to get a String representation of any "LispVal". The standard Haskell function "show" lets you convert any type that's an instance of the class Show into a String.
-- We'd like to be able to do the same with "LispVal", so we make it into a member of the class "Show", defining its "show" method as "showVal".

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal -- The "unwords" function glues together a list of words with spaces.

instance Show LispVal where show = showVal

-- It might be interesting to do a full treatment of the typeclass.