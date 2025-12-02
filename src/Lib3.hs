{-# OPTIONS_GHC -Wno-orphans #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp, Parser(..), parseCommand) where

import qualified Lib1

import Control.Concurrent.STM.TVar(TVar, readTVar, writeTVar, readTVarIO)
import Control.Concurrent.STM(atomically)
import Control.Concurrent (Chan, readChan, writeChan, newChan)
import Control.Applicative (Alternative(..))
import Data.Functor (($>))
import qualified System.IO.Strict as Strict
import System.IO(writeFile)
import Control.Exception(catch)
import System.IO.Error(IOError)

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

-- Functor instance
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> case p input of
        Left err -> Left errd
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
parseSequence = buildSequence <$> parseSimpleCommand <*> parseRestOfSequence
  where
    buildSequence :: Lib1.Command -> [Lib1.Command] -> Lib1.Command
    buildSequence cmd [] = cmd
    buildSequence cmd (c:cs) = buildSequence (Lib1.Sequence cmd c) cs
    
    parseRestOfSequence :: Parser [Lib1.Command]
    parseRestOfSequence = many (parseWhitespace *> parseChar ';' *> parseWhitespace *> parseSimpleCommand)

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
    Lib1.AddPassenger <$>
    (parseString "add" *>
     parseWhitespace1 *>
     parseString "passenger" *>
     parseWhitespace1 *>
     parsePlate) <*>
    (parseWhitespace1 *> parseSex) <*>
    (parseWhitespace1 *> parseNumber)

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
    ((parseChar '[' *> parseWhitespace *> parseChar ']') $> []) <|>
    (parseChar '[' *> parseWhitespace *> parsePassengerList <* parseWhitespace <* parseChar ']')

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

-- | State holds the collection of vehicles in our system
newtype State = State
  { vehicles :: [Lib1.Vehicle]
  } deriving (Show)

-- | Initial empty state
emptyState :: State
emptyState = State { vehicles = [] }

execute :: TVar State -> Lib1.Command -> IO ()
execute stateVar cmd = case cmd of
  Lib1.AddVehicle vehicle -> atomically $ do
    state <- readTVar stateVar
    let newState = addVehicle state vehicle
    writeTVar stateVar newState
  
  Lib1.FilterByPlate plate -> do
    state <- readTVarIO stateVar
    let result = filterByPlate state plate
    case result of
      Just v -> putStrLn $ "Found vehicle: " ++ showVehicle v
      Nothing -> putStrLn $ "No vehicle found with plate: " ++ plate
  
  Lib1.AddPassenger plate sex age -> atomically $ do
    state <- readTVar stateVar
    case addPassenger state plate sex age of
      Left err -> error err
      Right newState -> writeTVar stateVar newState
  
  Lib1.CalculateAverageAge vehicle -> do
    let avg = calculateAverageAge vehicle
    putStrLn $ "Average age: " ++ show avg
  
  Lib1.Sequence cmd1 cmd2 -> do
    execute stateVar cmd1
    execute stateVar cmd2
  
  Lib1.Dump Lib1.Examples -> do
    putStrLn "Examples:"
    mapM_ (putStrLn . ("  " ++) . toCliCommand) Lib1.examples

-- Pure domain logic functions

addVehicle :: State -> Lib1.Vehicle -> State
addVehicle state vehicle = 
  let filteredVehicles = filter (\v -> Lib1.vehiclePlate v /= Lib1.vehiclePlate vehicle) (vehicles state)
  in state { vehicles = vehicle : filteredVehicles }

filterByPlate :: State -> String -> Maybe Lib1.Vehicle
filterByPlate state plate = 
  case filter (\v -> Lib1.vehiclePlate v == plate) (vehicles state) of
    (v:_) -> Just v
    [] -> Nothing

addPassenger :: State -> String -> Char -> Int -> Either String State
addPassenger state plate sex age = 
  case filterByPlate state plate of
    Nothing -> Left $ "Vehicle with plate " ++ plate ++ " not found"
    Just vehicle -> 
      let newPassenger = Lib1.Passenger sex age
          updatedVehicle = vehicle { Lib1.passengers = Lib1.passengers vehicle ++ [newPassenger] }
          updatedVehicles = map (\v -> if Lib1.vehiclePlate v == plate then updatedVehicle else v) (vehicles state)
      in Right $ state { vehicles = updatedVehicles }

calculateAverageAge :: Lib1.Vehicle -> Double
calculateAverageAge vehicle =
  let driverAge = fromIntegral $ Lib1.driverAge $ Lib1.driver vehicle
      passengerAges = map (fromIntegral . Lib1.passengerAge) (Lib1.passengers vehicle)
      allAges = driverAge : passengerAges
      total = sum allAges
      count = length allAges
  in if count > 0 then total / fromIntegral count else 0.0

-- Helper function to convert command to CLI string
toCliCommand :: Lib1.Command -> String
toCliCommand (Lib1.AddVehicle vehicle) = "add vehicle " ++ showVehicle vehicle
toCliCommand (Lib1.FilterByPlate plate) = "filter by plate " ++ plate
toCliCommand (Lib1.AddPassenger plate sex age) = "add passenger " ++ plate ++ " " ++ [sex] ++ " " ++ show age
toCliCommand (Lib1.CalculateAverageAge vehicle) = "calculate average age " ++ showVehicle vehicle
toCliCommand (Lib1.Sequence cmd1 cmd2) = toCliCommand cmd1 ++ "; " ++ toCliCommand cmd2
toCliCommand (Lib1.Dump Lib1.Examples) = "dump examples"

showVehicle :: Lib1.Vehicle -> String
showVehicle (Lib1.Vehicle plate driver passengers) = 
  plate ++ " " ++ showDriver driver ++ " " ++ showPassengers passengers

showDriver :: Lib1.Driver -> String
showDriver (Lib1.Driver sex age) = [sex] ++ " " ++ show age

showPassengers :: [Lib1.Passenger] -> String
showPassengers [] = "[]"
showPassengers passengers = "[" ++ showPassengerList passengers ++ "]"

showPassengerList :: [Lib1.Passenger] -> String
showPassengerList [] = ""
showPassengerList [p] = showPassenger p
showPassengerList (p:ps) = showPassenger p ++ ", " ++ showPassengerList ps

showPassenger :: Lib1.Passenger -> String
showPassenger (Lib1.Passenger sex age) = [sex] ++ " " ++ show age

-- ============================================================================
-- Storage Operations
-- ============================================================================

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main in a dedicated thread. 
-- It must be used to control file access in a synchronized manner.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = loop
  where
    loop = do
      op <- readChan chan
      case op of
        Save content responseChan -> do
          writeFile "state.txt" content
          writeChan responseChan ()
          loop
        Load responseChan -> do
          content <- Strict.readFile "state.txt" `catch` handleNotFound
          writeChan responseChan content
          loop
    
    handleNotFound :: IOError -> IO String
    handleNotFound _ = return ""

-- | This function will be called periodically and on programs' exit. 
-- File writes must be performed through `Chan StorageOp`.
save :: Chan StorageOp -> TVar State -> IO (Either String ())
save chan stateVar = do
  state <- readTVarIO stateVar
  let commands = stateToCommands state
  let content = unlines $ map toCliCommand commands
  responseChan <- newChan
  writeChan chan (Save content responseChan)
  _ <- readChan responseChan
  return $ Right ()

-- | This function will be called on program start
-- File reads must be performed through `Chan StorageOp`
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load chan stateVar = do
  responseChan <- newChan
  writeChan chan (Load responseChan)
  content <- readChan responseChan
  if null content || all null (lines content)
  then do
    atomically $ writeTVar stateVar emptyState
    return $ Right ()
  else case parseAndExecuteCommands content of
    Left err -> return $ Left err
    Right newState -> do
      atomically $ writeTVar stateVar newState
      return $ Right ()

-- | Convert State to minimal list of commands (optimized)
stateToCommands :: State -> [Lib1.Command]
stateToCommands state = map Lib1.AddVehicle (vehicles state)

-- | Parse all commands from file and rebuild state
parseAndExecuteCommands :: String -> Either String State
parseAndExecuteCommands content = 
  let commandLines = filter (not . null) $ lines content
      parsedCommands = map (runParser parseCommand) commandLines
  in case sequence parsedCommands of
    Left err -> Left err
    Right commands -> Right $ foldl applyCommand emptyState (map fst commands)
  where
    applyCommand :: State -> Lib1.Command -> State
    applyCommand state (Lib1.AddVehicle vehicle) = addVehicle state vehicle
    applyCommand state (Lib1.AddPassenger plate sex age) = 
      case addPassenger state plate sex age of
        Right newState -> newState
        Left _ -> state
    applyCommand state (Lib1.Sequence cmd1 cmd2) = 
      applyCommand (applyCommand state cmd1) cmd2
    applyCommand state _ = state