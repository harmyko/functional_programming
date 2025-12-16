{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
module Lib4 (
    parseCommand,
    Parser,
    VehicleDSL(..),
    addVehicleDSL,
    filterByPlateDSL,
    addPassengerDSL,
    calculateAverageAgeDSL,
    sequenceDSL,
    dumpDSL,
    runHttpInterpreter,
    runStateInterpreter
) where

import qualified Lib1
import qualified Lib2
import qualified Lib3
import Test.QuickCheck (Arbitrary(..), Gen, elements, listOf, choose, oneof, sized)

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Free (Free, liftF, foldFree)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Applicative (Alternative(..))
import qualified Control.Applicative as A
import Data.Functor (($>))
import Network.HTTP.Simple (httpBS, parseRequest, getResponseBody, setRequestBodyLBS)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

type ErrorMsg = String
type Input = String
type Parser = ExceptT ErrorMsg (State Input)

-- ============================================================================
-- Parser Implementation (ExceptT + State monad)
-- ============================================================================

-- We'll use the standard many and some from Control.Applicative

-- | Backtracking combinator that restores the parser state on failure
tryParse :: Parser a -> Parser a
tryParse p = do
    st <- lift get
    result <- (Just <$> p) <|> pure Nothing
    case result of
        Just r -> return r
        Nothing -> do
            lift $ put st
            empty

-- | Parses user's input.
parseCommand :: Parser Lib1.Command
parseCommand = parseSequence

-- BNF: <command> ::= <simple-command> | <sequence>
-- BNF: <sequence> ::= <simple-command> ";" <command>
parseSequence :: Parser Lib1.Command
parseSequence = do
    firstCmd <- tryParse parseSimpleCommand
    parseSequenceHelper firstCmd
  where
    parseSequenceHelper :: Lib1.Command -> Parser Lib1.Command
    parseSequenceHelper cmd = do
        maybeSemicolon <- (Just <$> (parseWhitespace *> parseChar ';' <* parseWhitespace)) <|> pure Nothing
        case maybeSemicolon of
            Nothing -> return cmd
            Just _ -> do
                nextCmd <- tryParse parseSimpleCommand
                parseSequenceHelper (Lib1.Sequence cmd nextCmd)  -- Left-associative!

-- BNF: <simple-command> ::= <add-vehicle> | <filter-by-plate> | <add-passenger> | <calculate-average-age> | <dump>
parseSimpleCommand :: Parser Lib1.Command
parseSimpleCommand = 
    tryParse parseAddVehicle <|> 
    tryParse parseFilterByPlate <|> 
    tryParse parseAddPassenger <|> 
    tryParse parseCalculateAverageAge <|> 
    tryParse parseDump <|> 
    (lift get >>= \input -> throwE ("Failed to parse command: " ++ input))

-- BNF: <add-vehicle> ::= "add" <whitespace> "vehicle" <whitespace> <plate> <whitespace> <driver> <whitespace> <passengers>
parseAddVehicle :: Parser Lib1.Command
parseAddVehicle = do
    _ <- parseString "add"
    _ <- parseWhitespace1
    _ <- parseString "vehicle"
    _ <- parseWhitespace1
    vehicle <- parseVehicle
    return $ Lib1.AddVehicle vehicle

-- BNF: <filter-by-plate> ::= "filter" <whitespace> "by" <whitespace> "plate" <whitespace> <plate>
parseFilterByPlate :: Parser Lib1.Command
parseFilterByPlate = do
    _ <- parseString "filter"
    _ <- parseWhitespace1
    _ <- parseString "by"
    _ <- parseWhitespace1
    _ <- parseString "plate"
    _ <- parseWhitespace1
    plate <- parsePlate
    return $ Lib1.FilterByPlate plate

-- BNF: <add-passenger> ::= "add" <whitespace> "passenger" <whitespace> <plate> <whitespace> <sex> <whitespace> <age>
parseAddPassenger :: Parser Lib1.Command
parseAddPassenger = do
    _ <- parseString "add"
    _ <- parseWhitespace1
    _ <- parseString "passenger"
    _ <- parseWhitespace1
    plate <- parsePlate
    _ <- parseWhitespace1
    sex <- parseSex
    _ <- parseWhitespace1
    age <- parseNumber
    return $ Lib1.AddPassenger plate sex age

-- BNF: <calculate-average-age> ::= "calculate" <whitespace> "average" <whitespace> "age" <whitespace> <vehicle>
parseCalculateAverageAge :: Parser Lib1.Command
parseCalculateAverageAge = do
    _ <- parseString "calculate"
    _ <- parseWhitespace1
    _ <- parseString "average"
    _ <- parseWhitespace1
    _ <- parseString "age"
    _ <- parseWhitespace1
    vehicle <- parseVehicle
    return $ Lib1.CalculateAverageAge vehicle

-- BNF: <dump> ::= "dump" <whitespace> "examples"
parseDump :: Parser Lib1.Command
parseDump = do
    _ <- parseString "dump"
    _ <- parseWhitespace1
    _ <- parseString "examples"
    return $ Lib1.Dump Lib1.Examples

-- BNF: <vehicle> ::= <plate> <whitespace> <driver> <whitespace> <passengers>
parseVehicle :: Parser Lib1.Vehicle
parseVehicle = do
    plate <- parsePlate
    _ <- parseWhitespace1
    driver <- parseDriver
    _ <- parseWhitespace1
    passengers <- parsePassengers
    return $ Lib1.Vehicle plate driver passengers

-- BNF: <driver> ::= <sex> <whitespace> <age>
parseDriver :: Parser Lib1.Driver
parseDriver = do
    sex <- parseSex
    _ <- parseWhitespace1
    age <- parseNumber
    return $ Lib1.Driver sex age

-- BNF: <passengers> ::= "[" <whitespace> <passenger-list> <whitespace> "]" | "[" <whitespace> "]"
parsePassengers :: Parser [Lib1.Passenger]
parsePassengers = do
    _ <- parseChar '['
    _ <- parseWhitespace
    result <- (parseChar ']' $> []) <|> (parsePassengerList <* parseWhitespace <* parseChar ']')
    return result

-- BNF: <passenger-list> ::= <passenger> | <passenger> <whitespace> "," <whitespace> <passenger-list>
parsePassengerList :: Parser [Lib1.Passenger]
parsePassengerList = do
    passenger <- parsePassenger
    rest <- (parseWhitespace *> parseChar ',' *> parseWhitespace *> parsePassengerList) <|> pure []
    return (passenger : rest)

-- BNF: <passenger> ::= <sex> <whitespace> <age>
parsePassenger :: Parser Lib1.Passenger
parsePassenger = do
    sex <- parseSex
    _ <- parseWhitespace1
    age <- parseNumber
    return $ Lib1.Passenger sex age

-- BNF: <plate> ::= <letter> <letter> <letter> "-" <digit> <digit> <digit>
parsePlate :: Parser String
parsePlate = do
    input <- lift get
    if length input < 7
    then throwE "Plate too short"
    else let (letters, rest1) = splitAt 3 input
         in if all isLetterChar letters && not (null rest1) && head rest1 == '-'
            then let rest2 = tail rest1
                     (digits, rest3) = splitAt 3 rest2
                 in if length digits == 3 && all isDigitChar digits
                    then do
                        lift $ put rest3
                        return (letters ++ "-" ++ digits)
                    else throwE "Expected 3 digits after '-'"
            else throwE "Expected plate format ABC-123"

-- BNF: <sex> ::= 'M' | 'F'
parseSex :: Parser Char
parseSex = parseChar 'M' <|> parseChar 'F'

-- BNF: <number> ::= <digit> | <digit> <number>
parseNumber :: Parser Int
parseNumber = do
    input <- lift get
    case input of
        [] -> throwE "Expected number"
        _ -> case span isDigitChar input of
            ([], _) -> throwE "Expected number"
            (digits, rest) -> do
                lift $ put rest
                return (read digits)

-- Helper parsers
parseChar :: Char -> Parser Char
parseChar expected = do
    input <- lift get
    case input of
        (c:rest) | c == expected -> do
            lift $ put rest
            return c
        _ -> throwE $ "Expected '" ++ [expected] ++ "'"

parseString :: String -> Parser String
parseString expected = do
    input <- lift get
    if take (length expected) input == expected
    then do
        lift $ put (drop (length expected) input)
        return expected
    else throwE $ "Expected '" ++ expected ++ "'"

-- BNF: <whitespace> ::= " " | "\t" | "\n" | <whitespace> <whitespace>
parseWhitespace :: Parser String
parseWhitespace = do
    input <- lift get
    let (ws, rest) = span isWhitespaceChar input
    lift $ put rest
    return ws

-- Parse at least one whitespace character
parseWhitespace1 :: Parser String
parseWhitespace1 = do
    input <- lift get
    case input of
        [] -> throwE "Expected whitespace"
        (c:rest) | isWhitespaceChar c -> do
            let (ws, rest2) = span isWhitespaceChar rest
            lift $ put rest2
            return (c:ws)
        _ -> throwE "Expected whitespace"

-- Character predicates
isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

isLetterChar :: Char -> Bool
isLetterChar c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c == ' ' || c == '\t' || c == '\n'

-- ============================================================================
-- Arbitrary Instance for QuickCheck
-- ============================================================================

instance Arbitrary Lib1.Command where
  arbitrary :: Gen Lib1.Command
  arbitrary = sized arbCommand
    where
      arbCommand :: Int -> Gen Lib1.Command
      arbCommand 0 = arbSimpleCommand
      arbCommand n = oneof
        [ arbSimpleCommand
        , do  -- Generate left-associative sequences
            let n' = n `div` 2
            left <- arbCommand n'
            right <- arbSimpleCommand  -- Right side is always simple
            return $ Lib1.Sequence left right
        ]
      
      arbSimpleCommand :: Gen Lib1.Command
      arbSimpleCommand = oneof
        [ Lib1.AddVehicle <$> arbitrary
        , Lib1.FilterByPlate <$> arbitraryPlate
        , Lib1.AddPassenger <$> arbitraryPlate <*> arbitrarySex <*> arbitraryAge
        , Lib1.CalculateAverageAge <$> arbitrary
        , return (Lib1.Dump Lib1.Examples)
        ]

instance Arbitrary Lib1.Vehicle where
  arbitrary = Lib1.Vehicle <$> arbitraryPlate <*> arbitrary <*> arbitrary

instance Arbitrary Lib1.Driver where
  arbitrary = Lib1.Driver <$> arbitrarySex <*> arbitraryAge

instance Arbitrary Lib1.Passenger where
  arbitrary = Lib1.Passenger <$> arbitrarySex <*> arbitraryAge

arbitraryPlate :: Gen String
arbitraryPlate = do
    l1 <- elements ['A'..'Z']
    l2 <- elements ['A'..'Z']
    l3 <- elements ['A'..'Z']
    d1 <- elements ['0'..'9']
    d2 <- elements ['0'..'9']
    d3 <- elements ['0'..'9']
    return [l1, l2, l3, '-', d1, d2, d3]

arbitrarySex :: Gen Char
arbitrarySex = elements ['M', 'F']

arbitraryAge :: Gen Int
arbitraryAge = choose (1, 99)

instance Arbitrary Lib1.Dumpable where
  arbitrary = return Lib1.Examples

-- ============================================================================
-- Free Monad DSL
-- ============================================================================

-- | DSL for vehicle operations
data VehicleDSL next
  = AddVehicleDSL Lib1.Vehicle next
  | FilterByPlateDSL String (Maybe Lib1.Vehicle -> next)
  | AddPassengerDSL String Char Int (Either String () -> next)
  | CalculateAverageAgeDSL Lib1.Vehicle (Double -> next)
  | SequenceDSL (Free VehicleDSL ()) (Free VehicleDSL ()) next
  | DumpDSL ([String] -> next)
  deriving (Functor)

-- DSL smart constructors
addVehicleDSL :: Lib1.Vehicle -> Free VehicleDSL ()
addVehicleDSL v = liftF (AddVehicleDSL v ())

filterByPlateDSL :: String -> Free VehicleDSL (Maybe Lib1.Vehicle)
filterByPlateDSL plate = liftF (FilterByPlateDSL plate id)

addPassengerDSL :: String -> Char -> Int -> Free VehicleDSL (Either String ())
addPassengerDSL plate sex age = liftF (AddPassengerDSL plate sex age id)

calculateAverageAgeDSL :: Lib1.Vehicle -> Free VehicleDSL Double
calculateAverageAgeDSL v = liftF (CalculateAverageAgeDSL v id)

sequenceDSL :: Free VehicleDSL () -> Free VehicleDSL () -> Free VehicleDSL ()
sequenceDSL cmd1 cmd2 = liftF (SequenceDSL cmd1 cmd2 ())

dumpDSL :: Free VehicleDSL [String]
dumpDSL = liftF (DumpDSL id)

-- ============================================================================
-- HTTP Interpreter (sends commands to server)
-- ============================================================================

runHttpInterpreter :: String -> Free VehicleDSL a -> IO a
runHttpInterpreter serverUrl = foldFree interpret
  where
    interpret :: VehicleDSL a -> IO a
    interpret (AddVehicleDSL vehicle next) = do
        let cmd = Lib2.toCliCommand (Lib1.AddVehicle vehicle)
        _ <- sendCommand cmd
        return next
    
    interpret (FilterByPlateDSL plate cont) = do
        let cmd = Lib2.toCliCommand (Lib1.FilterByPlate plate)
        response <- sendCommand cmd
        -- Parse response to extract vehicle if found
        let result = parseFilterResponse response
        return (cont result)
    
    interpret (AddPassengerDSL plate sex age cont) = do
        let cmd = Lib2.toCliCommand (Lib1.AddPassenger plate sex age)
        response <- sendCommand cmd
        let result = if "error" `elem` words (map toLower response)
                     then Left response
                     else Right ()
        return (cont result)
    
    interpret (CalculateAverageAgeDSL vehicle cont) = do
        let cmd = Lib2.toCliCommand (Lib1.CalculateAverageAge vehicle)
        response <- sendCommand cmd
        let avg = parseAverageResponse response
        return (cont avg)
    
    interpret (SequenceDSL cmd1 cmd2 next) = do
        _ <- foldFree interpret cmd1
        _ <- foldFree interpret cmd2
        return next
    
    interpret (DumpDSL cont) = do
        let cmd = Lib2.toCliCommand (Lib1.Dump Lib1.Examples)
        response <- sendCommand cmd
        return (cont (lines response))
    
    sendCommand :: String -> IO String
    sendCommand cmd = do
        request <- parseRequest serverUrl
        let request' = setRequestBodyLBS (LBS.pack cmd) request
        response <- httpBS request'
        return $ BS.unpack $ getResponseBody response
    
    parseFilterResponse :: String -> Maybe Lib1.Vehicle
    parseFilterResponse resp
        | "No vehicle found" `isInfixOf` resp = Nothing
        | "Found vehicle:" `isInfixOf` resp = 
            -- Extract vehicle data from response
            let vehicleStr = drop (length "Found vehicle: ") resp
            in case runState (runExceptT parseVehicle) vehicleStr of
                (Right v, _) -> Just v
                _ -> Nothing
        | otherwise = Nothing
    
    parseAverageResponse :: String -> Double
    parseAverageResponse resp =
        case words resp of
            ("Average":"age:":avgStr:_) -> read avgStr
            _ -> 0.0
    
    toLower :: Char -> Char
    toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
              | otherwise = c
    
    isInfixOf :: String -> String -> Bool
    isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
    
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

-- ============================================================================
-- State Interpreter (local execution with State monad)
-- ============================================================================

runStateInterpreter :: Lib3.State -> Free VehicleDSL a -> (a, Lib3.State)
runStateInterpreter initialState program = 
    runState (foldFree interpret program) initialState
  where
    interpret :: VehicleDSL a -> State Lib3.State a
    interpret (AddVehicleDSL vehicle next) = do
        state <- get
        let newState = Lib3.addVehicle state vehicle
        put newState
        return next
    
    interpret (FilterByPlateDSL plate cont) = do
        state <- get
        let result = Lib3.filterByPlate state plate
        return (cont result)
    
    interpret (AddPassengerDSL plate sex age cont) = do
        state <- get
        let result = Lib3.addPassenger state plate sex age
        case result of
            Right newState -> do
                put newState
                return (cont (Right ()))
            Left err -> return (cont (Left err))
    
    interpret (CalculateAverageAgeDSL vehicle cont) = do
        let avg = Lib3.calculateAverageAge vehicle
        return (cont avg)
    
    interpret (SequenceDSL cmd1 cmd2 next) = do
        _ <- foldFree interpret cmd1
        _ <- foldFree interpret cmd2
        return next
    
    interpret (DumpDSL cont) = do
        let examples = map Lib2.toCliCommand Lib1.examples
        return (cont ("Examples:" : map ("  " ++) examples))

-- Helper functions that need to be accessible
addVehicle :: Lib3.State -> Lib1.Vehicle -> Lib3.State
addVehicle = Lib3.addVehicle

filterByPlate :: Lib3.State -> String -> Maybe Lib1.Vehicle
filterByPlate = Lib3.filterByPlate

addPassenger :: Lib3.State -> String -> Char -> Int -> Either String Lib3.State
addPassenger = Lib3.addPassenger

calculateAverageAge :: Lib1.Vehicle -> Double
calculateAverageAge = Lib3.calculateAverageAge
