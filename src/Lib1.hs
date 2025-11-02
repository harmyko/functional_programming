module Lib1
    ( examples, Command(..), Dumpable(..)
    , Vehicle(..), Driver(..), Passenger(..)
    ) where

data Vehicle = Vehicle
  { vehiclePlate :: String
  , driver :: Driver
  , passengers :: [Passenger]
  } deriving Show

data Driver = Driver
  { driverSex :: Char
  , driverAge :: Int
  } deriving Show

data Passenger = Passenger
  { passengerSex :: Char
  , passengerAge :: Int
  } deriving Show

data Command
  = AddVehicle Vehicle
  | FilterByPlate String
  | AddPassenger String Char Int
  | Sequence Command Command
  | CalculateAverageAge Vehicle
  | Dump Dumpable
  deriving Show

data Dumpable 
  = Examples
  deriving Show

vehicle0 :: Vehicle
vehicle0 = Vehicle
  { vehiclePlate = "ABC-123"
  , driver = Driver { driverSex = 'M', driverAge = 35 }
  , passengers =
      [ ]
  }

vehicle1 :: Vehicle
vehicle1 = Vehicle
  { vehiclePlate = "XYZ-789"
  , driver = Driver { driverSex = 'F', driverAge = 28 }
  , passengers =
      [ Passenger { passengerSex = 'M', passengerAge = 40 }
      , Passenger { passengerSex = 'F', passengerAge = 25 }
      ]
  }

vehicle2 :: Vehicle
vehicle2 = Vehicle
  { vehiclePlate = "LMN-456"
  , driver = Driver { driverSex = 'M', driverAge = 50 }
  , passengers =
      [ Passenger { passengerSex = 'F', passengerAge = 22 } ]
  }

example0 :: Command
example0 = AddVehicle vehicle0

example1 :: Command
example1 = FilterByPlate "LMN-456"

example2 :: Command
example2 = AddPassenger "ABC-123" 'F' 19

example3 :: Command
example3 = CalculateAverageAge vehicle2

example4 :: Command
example4 = Sequence 
  (Sequence 
    (AddVehicle vehicle0)
    (AddVehicle vehicle1)
  )
  (AddVehicle vehicle2)

examples :: [Command]
examples = [example0, example1, example2, example3, example4, Dump Examples]