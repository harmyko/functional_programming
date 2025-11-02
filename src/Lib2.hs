{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- | Parses user's input.
parseCommand :: Parser Lib1.Command
parseCommand = parseSequence

-- BNF: <command> ::= <simple-command> | <sequence>
-- BNF: <sequence> ::= <simple-command> ";" <command>
parseSequence :: Parser Lib1.Command
parseSequence input = case parseSimpleCommand input of
  Left err -> Left err
  Right (cmd, rest) -> parseSequenceHelper cmd rest

parseSequenceHelper :: Lib1.Command -> String -> Either ErrorMsg (Lib1.Command, String)
parseSequenceHelper cmd input = case parseWhitespace input of
  Right (_, ';':rest) -> 
    case parseWhitespace rest of
      Right (_, rest2) -> case parseSimpleCommand rest2 of
        Left err -> Left err
        Right (cmd2, rest3) -> parseSequenceHelper (Lib1.Sequence cmd cmd2) rest3
      Left err -> Left err
  Right (_, rest) -> Right (cmd, rest)
  Left err -> Left err

-- BNF: <simple-command> ::= <add-vehicle> | <filter-by-plate> | <add-passenger> | <calculate-average-age> | <dump>
parseSimpleCommand :: Parser Lib1.Command
parseSimpleCommand = orElse (orElse (orElse (orElse
  parseAddVehicle
  parseFilterByPlate)
  parseAddPassenger)
  parseCalculateAverageAge)
  parseDump

-- BNF: <add-vehicle> ::= "add" <whitespace> "vehicle" <whitespace> <plate> <whitespace> <driver> <whitespace> <passengers>
parseAddVehicle :: Parser Lib1.Command
parseAddVehicle input =
  and3 (parseString "add") parseWhitespace1 (parseString "vehicle") input
    >>= \((_, _), rest) -> case parseWhitespace1 rest of
      Left err -> Left err
      Right (_, rest2) -> case parseVehicle rest2 of
        Left err -> Left err
        Right (vehicle, rest3) -> Right (Lib1.AddVehicle vehicle, rest3)

-- BNF: <filter-by-plate> ::= "filter" <whitespace> "by" <whitespace> "plate" <whitespace> <plate>
parseFilterByPlate :: Parser Lib1.Command
parseFilterByPlate input =
  and5 (parseString "filter") parseWhitespace1 (parseString "by") parseWhitespace1 (parseString "plate") input
    >>= \((((_, _), _), _), rest) -> case parseWhitespace1 rest of
      Left err -> Left err
      Right (_, rest2) -> case parsePlate rest2 of
        Left err -> Left err
        Right (plate, rest3) -> Right (Lib1.FilterByPlate plate, rest3)

-- BNF: <add-passenger> ::= "add" <whitespace> "passenger" <whitespace> <plate> <whitespace> <sex> <whitespace> <age>
parseAddPassenger :: Parser Lib1.Command
parseAddPassenger input =
  and3 (parseString "add") parseWhitespace1 (parseString "passenger") input
    >>= \((_, _), rest) -> case parseWhitespace1 rest of
      Left err -> Left err
      Right (_, rest2) -> case parsePlate rest2 of
        Left err -> Left err
        Right (plate, rest3) -> case parseWhitespace1 rest3 of
          Left err -> Left err
          Right (_, rest4) -> case parseSex rest4 of
            Left err -> Left err
            Right (sex, rest5) -> case parseWhitespace1 rest5 of
              Left err -> Left err
              Right (_, rest6) -> case parseNumber rest6 of
                Left err -> Left err
                Right (age, rest7) -> Right (Lib1.AddPassenger plate sex age, rest7)

-- BNF: <calculate-average-age> ::= "calculate" <whitespace> "average" <whitespace> "age" <whitespace> <vehicle>
parseCalculateAverageAge :: Parser Lib1.Command
parseCalculateAverageAge input =
  and5 (parseString "calculate") parseWhitespace1 (parseString "average") parseWhitespace1 (parseString "age") input
    >>= \((((_, _), _), _), rest) -> case parseWhitespace1 rest of
      Left err -> Left err
      Right (_, rest2) -> case parseVehicle rest2 of
        Left err -> Left err
        Right (vehicle, rest3) -> Right (Lib1.CalculateAverageAge vehicle, rest3)

-- BNF: <dump> ::= "dump" <whitespace> "examples"
parseDump :: Parser Lib1.Command
parseDump input =
  and3 (parseString "dump") parseWhitespace1 (parseString "examples") input
    >>= \((_, _), rest) -> Right (Lib1.Dump Lib1.Examples, rest)

-- BNF: <vehicle> ::= <plate> <whitespace> <driver> <whitespace> <passengers>
parseVehicle :: Parser Lib1.Vehicle
parseVehicle input = case parsePlate input of
  Left err -> Left err
  Right (plate, rest) -> case parseWhitespace1 rest of
    Left err -> Left err
    Right (_, rest2) -> case parseDriver rest2 of
      Left err -> Left err
      Right (driver, rest3) -> case parseWhitespace1 rest3 of
        Left err -> Left err
        Right (_, rest4) -> case parsePassengers rest4 of
          Left err -> Left err
          Right (passengers, rest5) -> Right (Lib1.Vehicle plate driver passengers, rest5)

-- BNF: <driver> ::= <sex> <whitespace> <age>
parseDriver :: Parser Lib1.Driver
parseDriver input = case parseSex input of
  Left err -> Left err
  Right (sex, rest) -> case parseWhitespace1 rest of
    Left err -> Left err
    Right (_, rest2) -> case parseNumber rest2 of
      Left err -> Left err
      Right (age, rest3) -> Right (Lib1.Driver sex age, rest3)

-- BNF: <passengers> ::= "[" <whitespace> <passenger-list> <whitespace> "]" | "[" <whitespace> "]"
parsePassengers :: Parser [Lib1.Passenger]
parsePassengers [] = Left "Expected '['"
parsePassengers ('[':rest) = case parseWhitespace rest of
  Right (_, ']':rest2) -> Right ([], rest2)
  Right (_, rest2) -> case parsePassengerList rest2 of
    Left err -> Left err
    Right (passengers, rest3) -> case parseWhitespace rest3 of
      Right (_, ']':rest4) -> Right (passengers, rest4)
      _ -> Left "Expected ']'"
  Left err -> Left err
parsePassengers _ = Left "Expected '['"

-- BNF: <passenger-list> ::= <passenger> | <passenger> <whitespace> "," <whitespace> <passenger-list>
parsePassengerList :: Parser [Lib1.Passenger]
parsePassengerList input = case parsePassenger input of
  Left err -> Left err
  Right (passenger, rest) -> case parseWhitespace rest of
    Right (_, ',':rest2) -> case parseWhitespace rest2 of
      Right (_, rest3) -> case parsePassengerList rest3 of
        Left err -> Left err
        Right (passengers, rest4) -> Right (passenger:passengers, rest4)
      Left err -> Left err
    Right (_, rest2) -> Right ([passenger], rest2)
    Left err -> Left err

-- BNF: <passenger> ::= <sex> <whitespace> <age>
parsePassenger :: Parser Lib1.Passenger
parsePassenger input = case parseSex input of
  Left err -> Left err
  Right (sex, rest) -> case parseWhitespace1 rest of
    Left err -> Left err
    Right (_, rest2) -> case parseNumber rest2 of
      Left err -> Left err
      Right (age, rest3) -> Right (Lib1.Passenger sex age, rest3)

-- BNF: <plate> ::= <letter> <letter> <letter> "-" <digit> <digit> <digit>
parsePlate :: Parser String
parsePlate input
  | length input < 7 = Left "Plate too short"
  | otherwise = 
      let (letters, rest1) = splitAt 3 input
      in if all isLetterChar letters && not (null rest1) && head rest1 == '-'
         then let rest2 = tail rest1
                  (digits, rest3) = splitAt 3 rest2
              in if length digits == 3 && all isDigitChar digits
                 then Right (letters ++ "-" ++ digits, rest3)
                 else Left "Expected 3 digits after '-'"
         else Left "Expected plate format ABC-123"

-- BNF: <sex> ::= 'M' | 'F'
parseSex :: Parser Char
parseSex ('M':rest) = Right ('M', rest)
parseSex ('F':rest) = Right ('F', rest)
parseSex _ = Left "Expected 'M' or 'F'"

-- BNF: <number> ::= <digit> | <digit> <number>
parseNumber :: Parser Int
parseNumber [] = Left "Expected number"
parseNumber input = 
  case span isDigitChar input of
    ([], _) -> Left "Expected number"
    (digits, rest) -> Right (read digits, rest)

isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

isLetterChar :: Char -> Bool
isLetterChar c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

-- BNF: <string> ::= <char> | <char> <string>
parseString :: String -> Parser String
parseString expected input
  | take (length expected) input == expected = Right (expected, drop (length expected) input)
  | otherwise = Left $ "Expected '" ++ expected ++ "'"

-- BNF: <whitespace> ::= " " | "\t" | "\n" | <whitespace> <whitespace>
parseWhitespace :: Parser String
parseWhitespace input = 
  let (ws, rest) = span isWhitespaceChar input
  in Right (ws, rest)

-- Parse at least one whitespace character
parseWhitespace1 :: Parser String
parseWhitespace1 [] = Left "Expected whitespace"
parseWhitespace1 (c:rest)
  | isWhitespaceChar c = 
      let (ws, rest2) = span isWhitespaceChar rest
      in Right (c:ws, rest2)
  | otherwise = Left "Expected whitespace"

isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c == ' ' || c == '\t' || c == '\n'

-- Parser combinators
orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input = case p1 input of
  Right result -> Right result
  Left _ -> p2 input

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 input = case p1 input of
  Left err -> Left err
  Right (r1, rest1) -> case p2 rest1 of
    Left err -> Left err
    Right (r2, rest2) -> Right ((r1, r2), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser ((a, b), c)
and3 p1 p2 = and2 (and2 p1 p2)

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (((a, b), c), d)
and4 p1 p2 p3 = and2 (and3 p1 p2 p3)

and5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser ((((a, b), c), d), e)
and5 p1 p2 p3 p4 = and2 (and4 p1 p2 p3 p4)

-- Process function
process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

-- ToCliCommand instance
class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.AddVehicle vehicle) = "add vehicle " ++ vehicleToString vehicle
  toCliCommand (Lib1.FilterByPlate plate) = "filter by plate " ++ plate
  toCliCommand (Lib1.AddPassenger plate sex age) = "add passenger " ++ plate ++ " " ++ [sex] ++ " " ++ show age
  toCliCommand (Lib1.CalculateAverageAge vehicle) = "calculate average age " ++ vehicleToString vehicle
  toCliCommand (Lib1.Sequence cmd1 cmd2) = toCliCommand cmd1 ++ "; " ++ toCliCommand cmd2
  toCliCommand (Lib1.Dump Lib1.Examples) = "dump examples"

vehicleToString :: Lib1.Vehicle -> String
vehicleToString (Lib1.Vehicle plate driver passengers) = 
  plate ++ " " ++ driverToString driver ++ " " ++ passengersToString passengers

driverToString :: Lib1.Driver -> String
driverToString (Lib1.Driver sex age) = [sex] ++ " " ++ show age

passengersToString :: [Lib1.Passenger] -> String
passengersToString [] = "[]"
passengersToString passengers = "[" ++ passengerListToString passengers ++ "]"

passengerListToString :: [Lib1.Passenger] -> String
passengerListToString [] = ""
passengerListToString [p] = passengerToString p
passengerListToString (p:ps) = passengerToString p ++ ", " ++ passengerListToString ps

passengerToString :: Lib1.Passenger -> String
passengerToString (Lib1.Passenger sex age) = [sex] ++ " " ++ show age

-- Eq instance for Command
instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  (Lib1.AddVehicle v1) == (Lib1.AddVehicle v2) = v1 == v2
  (Lib1.FilterByPlate p1) == (Lib1.FilterByPlate p2) = p1 == p2
  (Lib1.AddPassenger p1 s1 a1) == (Lib1.AddPassenger p2 s2 a2) = p1 == p2 && s1 == s2 && a1 == a2
  (Lib1.CalculateAverageAge v1) == (Lib1.CalculateAverageAge v2) = v1 == v2
  (Lib1.Sequence cmd1a cmd1b) == (Lib1.Sequence cmd2a cmd2b) = cmd1a == cmd2a && cmd1b == cmd2b
  (Lib1.Dump d1) == (Lib1.Dump d2) = d1 == d2
  _ == _ = False

-- Need Eq instances for Vehicle, Driver, Passenger, Dumpable
instance Eq Lib1.Vehicle where
  (Lib1.Vehicle p1 d1 ps1) == (Lib1.Vehicle p2 d2 ps2) = p1 == p2 && d1 == d2 && ps1 == ps2

instance Eq Lib1.Driver where
  (Lib1.Driver s1 a1) == (Lib1.Driver s2 a2) = s1 == s2 && a1 == a2

instance Eq Lib1.Passenger where
  (Lib1.Passenger s1 a1) == (Lib1.Passenger s2 a2) = s1 == s2 && a1 == a2

instance Eq Lib1.Dumpable where
  Lib1.Examples == Lib1.Examples = True