{-# OPTIONS_GHC -Wno-orphans #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp, Parser(..), parseCommand) where

import qualified Lib1

import Control.Concurrent.STM.TVar(TVar)
import Control.Concurrent (Chan, readChan)
import Control.Applicative (Alternative(..))

import System.IO.Strict(readFile)
import System.IO(writeFile)
import Prelude hiding (readFile, writeFile)

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

-- Functor instance
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> case p input of
        Left err -> Left err
        Right (result, rest) -> Right (f result, rest)

-- Applicative instance
instance Applicative Parser where
    pure x = Parser $ \input -> Right (x, input)
    
    (Parser pf) <*> (Parser px) = Parser $ \input -> case pf input of
        Left err -> Left err
        Right (f, rest1) -> case px rest1 of
            Left err -> Left err
            Right (x, rest2) -> Right (f x, rest2)

-- Alternative instance
instance Alternative Parser where
    empty = Parser $ \_ -> Left "empty parser"
    
    (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
        Right result -> Right result
        Left _ -> p2 input

-- | Parses user's input.
parseCommand :: Parser Lib1.Command
parseCommand = parseSequence

-- BNF: <command> ::= <simple-command> | <sequence>
-- BNF: <sequence> ::= <simple-command> ";" <command>
parseSequence :: Parser Lib1.Command
parseSequence = parseSimpleCommand >>= parseSequenceHelper
  where
    parseSequenceHelper :: Lib1.Command -> Parser Lib1.Command
    parseSequenceHelper cmd = 
      ((\_ _ _ cmd2 -> Lib1.Sequence cmd cmd2) <$> 
        parseWhitespace <*> 
        parseChar ';' <*> 
        parseWhitespace <*> 
        parseSimpleCommand >>= parseSequenceHelper)
      <|> pure cmd

-- BNF: <simple-command> ::= <add-vehicle> | <filter-by-plate> | <add-passenger> | <calculate-average-age> | <dump>
parseSimpleCommand :: Parser Lib1.Command
parseSimpleCommand = 
    parseAddVehicle
    <|> parseFilterByPlate
    <|> parseAddPassenger
    <|> parseCalculateAverageAge
    <|> parseDump

-- BNF: <add-vehicle> ::= "add" <whitespace> "vehicle" <whitespace> <plate> <whitespace> <driver> <whitespace> <passengers>
parseAddVehicle :: Parser Lib1.Command
parseAddVehicle = 
    Lib1.AddVehicle <$> 
    (parseString "add" *> 
     parseWhitespace1 *> 
     parseString "vehicle" *> 
     parseWhitespace1 *> 
     parseVehicle)

-- BNF: <filter-by-plate> ::= "filter" <whitespace> "by" <whitespace> "plate" <whitespace> <plate>
parseFilterByPlate :: Parser Lib1.Command
parseFilterByPlate = 
    Lib1.FilterByPlate <$> 
    (parseString "filter" *> 
     parseWhitespace1 *> 
     parseString "by" *> 
     parseWhitespace1 *> 
     parseString "plate" *> 
     parseWhitespace1 *> 
     parsePlate)

-- BNF: <add-passenger> ::= "add" <whitespace> "passenger" <whitespace> <plate> <whitespace> <sex> <whitespace> <age>
parseAddPassenger :: Parser Lib1.Command
parseAddPassenger = 
    (\_ _ _ plate _ sex _ age -> Lib1.AddPassenger plate sex age) <$>
    parseString "add" <*>
    parseWhitespace1 <*>
    parseString "passenger" <*>
    parseWhitespace1 *>
    parsePlate <*>
    parseWhitespace1 <*>
    parseSex <*>
    parseWhitespace1 <*>
    parseNumber

-- BNF: <calculate-average-age> ::= "calculate" <whitespace> "average" <whitespace> "age" <whitespace> <vehicle>
parseCalculateAverageAge :: Parser Lib1.Command
parseCalculateAverageAge = 
    Lib1.CalculateAverageAge <$>
    (parseString "calculate" *>
     parseWhitespace1 *>
     parseString "average" *>
     parseWhitespace1 *>
     parseString "age" *>
     parseWhitespace1 *>
     parseVehicle)

-- BNF: <dump> ::= "dump" <whitespace> "examples"
parseDump :: Parser Lib1.Command
parseDump = 
    Lib1.Dump Lib1.Examples <$
    (parseString "dump" *>
     parseWhitespace1 *>
     parseString "examples")

-- BNF: <vehicle> ::= <plate> <whitespace> <driver> <whitespace> <passengers>
parseVehicle :: Parser Lib1.Vehicle
parseVehicle = 
    Lib1.Vehicle <$>
    parsePlate <*>
    (parseWhitespace1 *> parseDriver) <*>
    (parseWhitespace1 *> parsePassengers)

-- BNF: <driver> ::= <sex> <whitespace> <age>
parseDriver :: Parser Lib1.Driver
parseDriver = 
    Lib1.Driver <$>
    parseSex <*>
    (parseWhitespace1 *> parseNumber)

-- BNF: <passengers> ::= "[" <whitespace> <passenger-list> <whitespace> "]" | "[" <whitespace> "]"
parsePassengers :: Parser [Lib1.Passenger]
parsePassengers = 
    (parseChar '[' *> parseWhitespace *> parseChar ']' *> pure [])
    <|> (parseChar '[' *> parseWhitespace *> parsePassengerList <* parseWhitespace <* parseChar ']')

-- BNF: <passenger-list> ::= <passenger> | <passenger> <whitespace> "," <whitespace> <passenger-list>
parsePassengerList :: Parser [Lib1.Passenger]
parsePassengerList = 
    (:) <$> parsePassenger <*> 
    ((parseWhitespace *> parseChar ',' *> parseWhitespace *> parsePassengerList) <|> pure [])

-- BNF: <passenger> ::= <sex> <whitespace> <age>
parsePassenger :: Parser Lib1.Passenger
parsePassenger = 
    Lib1.Passenger <$>
    parseSex <*>
    (parseWhitespace1 *> parseNumber)

-- BNF: <plate> ::= <letter> <letter> <letter> "-" <digit> <digit> <digit>
parsePlate :: Parser String
parsePlate = Parser $ \input ->
    if length input < 7
    then Left "Plate too short"
    else let (letters, rest1) = splitAt 3 input
         in if all isLetterChar letters && not (null rest1) && head rest1 == '-'
            then let rest2 = tail rest1
                     (digits, rest3) = splitAt 3 rest2
                 in if length digits == 3 && all isDigitChar digits
                    then Right (letters ++ "-" ++ digits, rest3)
                    else Left "Expected 3 digits after '-'"
            else Left "Expected plate format ABC-123"

-- BNF: <sex> ::= 'M' | 'F'
parseSex :: Parser Char
parseSex = parseChar 'M' <|> parseChar 'F'

-- BNF: <number> ::= <digit> | <digit> <number>
parseNumber :: Parser Int
parseNumber = Parser $ \input -> case input of
    [] -> Left "Expected number"
    _ -> case span isDigitChar input of
        ([], _) -> Left "Expected number"
        (digits, rest) -> Right (read digits, rest)

-- Helper parsers
parseChar :: Char -> Parser Char
parseChar expected = Parser $ \input -> case input of
    (c:rest) | c == expected -> Right (c, rest)
    _ -> Left $ "Expected '" ++ [expected] ++ "'"

parseString :: String -> Parser String
parseString expected = Parser $ \input ->
    if take (length expected) input == expected
    then Right (expected, drop (length expected) input)
    else Left $ "Expected '" ++ expected ++ "'"

-- BNF: <whitespace> ::= " " | "\t" | "\n" | <whitespace> <whitespace>
parseWhitespace :: Parser String
parseWhitespace = Parser $ \input ->
    let (ws, rest) = span isWhitespaceChar input
    in Right (ws, rest)

-- Parse at least one whitespace character
parseWhitespace1 :: Parser String
parseWhitespace1 = Parser $ \input -> case input of
    [] -> Left "Expected whitespace"
    (c:rest) | isWhitespaceChar c ->
        let (ws, rest2) = span isWhitespaceChar rest
        in Right (c:ws, rest2)
    _ -> Left "Expected whitespace"

-- Character predicates
isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

isLetterChar :: Char -> Bool
isLetterChar c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c == ' ' || c == '\t' || c == '\n'

-- Note: We need a bind operation for parseSequenceHelper, but we can implement
-- it as a standalone function without making Parser a Monad instance
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
(Parser p) >>= f = Parser $ \input -> case p input of
    Left err -> Left err
    Right (result, rest) -> runParser (f result) rest

-- | You can change the type to whatever needed. If your domain
-- does not have any state you have to make it up.
newtype State = State ()

-- Fix this accordingly
emptyState :: State
emptyState = State()

-- | Business/domain logic happens here.
-- This function makes your program actually usefull.
-- You may print if you want to print, you
-- may mutate state if needed but there must be
-- SINGLE atomically call in the function
-- You do not want to write/read files here.
execute :: TVar State -> Lib1.Command -> IO ()
execute _ _ = error "Implement me 1"

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request. It must run forever.
-- Modify as needed.
-- You might want to use readFile from `strict` library
-- if you get "resource locked" exceptions under Windows.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop c = do
  _ <- readChan c
  return $ error "Implement me 2"

-- | This function will be called periodically
-- and on programs' exit. File writes must be performed
-- through `Chan StorageOp`.
save :: Chan StorageOp -> TVar State -> IO (Either String ())
save _ _ = return $ Left "Implement me 3"

-- | This function will be called on program start
-- File reads must be performed through `Chan StorageOp`
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load _ _ = return $ Left "Implement me 4"