module Lib1
    ( examples, Command(..), Dumpable(..)
    ) where

data Vehicle = Vehicle
  { vehiclePlate :: String
  , driver :: Driver
  , passengers :: [Passenger]
  } deriving Show

data Driver = Driver
  { driverSex :: String
  , driverAge :: Int
  } deriving Show

data Passenger = Passenger
  { passengerSex :: String
  , passengerAge :: Int
  } deriving Show

data Command
  = AddVehicle Vehicle
  | FilterByPlate String
  | Sequence Command Command
  | Dump Dumpable
  deriving Show

data Dumpable 
  = Examples
  deriving Show

vehicle0 :: Vehicle
vehicle0 = Vehicle
  { vehiclePlate = "ABC-123"
  , driver = Driver { driverSex = "M", driverAge = 35 }
  , passengers =
      [ ]
  }

vehicle1 :: Vehicle
vehicle1 = Vehicle
  { vehiclePlate = "XYZ-789"
  , driver = Driver { driverSex = "F", driverAge = 28 }
  , passengers =
      [ Passenger { passengerSex = "M", passengerAge = 40 }
      , Passenger { passengerSex = "F", passengerAge = 25 }
      ]
  }

vehicle2 :: Vehicle
vehicle2 = Vehicle
  { vehiclePlate = "LMN-456"
  , driver = Driver { driverSex = "M", driverAge = 50 }
  , passengers =
      [ Passenger { passengerSex = "F", passengerAge = 22 } ]
  }

example0 :: Command
example0 = Sequence
  (AddVehicle vehicle0)
  (AddVehicle vehicle1) 

example1 :: Command
example1 = Sequence
  example0
  (AddVehicle vehicle2)

examples :: [Command]
examples = [example0, example1, Dump Examples]
