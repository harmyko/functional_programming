module Main (main) where

import qualified Lib1
import qualified Lib3
import qualified Lib4
import Control.Monad.Free (Free)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "Vehicle Management System - Free Monad DSL Client"
    putStrLn "===================================================\n"
    
    putStrLn "Choose interpreter:"
    putStrLn "1. HTTP Interpreter (connects to server)"
    putStrLn "2. State Interpreter (local execution)"
    putStr "Enter choice (1 or 2): "
    hFlush stdout
    choice <- getLine
    
    case choice of
        "1" -> runHttpExample
        "2" -> runStateExample
        _ -> putStrLn "Invalid choice"

-- ============================================================================
-- Example DSL Programs
-- ============================================================================

-- Example 1: Add a vehicle
exampleAddVehicle :: Free Lib4.VehicleDSL ()
exampleAddVehicle = do
    let vehicle = Lib1.Vehicle "ABC-123" (Lib1.Driver 'M' 35) []
    Lib4.addVehicleDSL vehicle
    putStrLnDSL "Added vehicle ABC-123"

-- Example 2: Add vehicle and filter
exampleAddAndFilter :: Free Lib4.VehicleDSL ()
exampleAddAndFilter = do
    let vehicle = Lib1.Vehicle "XYZ-789" (Lib1.Driver 'F' 28) 
                    [Lib1.Passenger 'M' 40]
    Lib4.addVehicleDSL vehicle
    putStrLnDSL "Added vehicle XYZ-789"
    
    result <- Lib4.filterByPlateDSL "XYZ-789"
    case result of
        Just v -> putStrLnDSL $ "Found: " ++ Lib1.vehiclePlate v
        Nothing -> putStrLnDSL "Vehicle not found"

-- Example 3: Add passenger to existing vehicle
exampleAddPassenger :: Free Lib4.VehicleDSL ()
exampleAddPassenger = do
    let vehicle = Lib1.Vehicle "DEF-456" (Lib1.Driver 'M' 45) []
    Lib4.addVehicleDSL vehicle
    putStrLnDSL "Added vehicle DEF-456"
    
    result <- Lib4.addPassengerDSL "DEF-456" 'F' 22
    case result of
        Right () -> putStrLnDSL "Passenger added successfully"
        Left err -> putStrLnDSL $ "Error: " ++ err

-- Example 4: Calculate average age
exampleCalculateAverage :: Free Lib4.VehicleDSL ()
exampleCalculateAverage = do
    let vehicle = Lib1.Vehicle "GHI-789" (Lib1.Driver 'F' 30) 
                    [Lib1.Passenger 'M' 25, Lib1.Passenger 'F' 20]
    avg <- Lib4.calculateAverageAgeDSL vehicle
    putStrLnDSL $ "Average age: " ++ show avg

-- Example 5: Complex sequence
exampleComplex :: Free Lib4.VehicleDSL ()
exampleComplex = do
    -- Add first vehicle
    let vehicle1 = Lib1.Vehicle "AAA-111" (Lib1.Driver 'M' 40) []
    Lib4.addVehicleDSL vehicle1
    putStrLnDSL "Added vehicle AAA-111"
    
    -- Add second vehicle
    let vehicle2 = Lib1.Vehicle "BBB-222" (Lib1.Driver 'F' 35) 
                    [Lib1.Passenger 'M' 10]
    Lib4.addVehicleDSL vehicle2
    putStrLnDSL "Added vehicle BBB-222"
    
    -- Add passenger to first vehicle
    _ <- Lib4.addPassengerDSL "AAA-111" 'F' 38
    putStrLnDSL "Added passenger to AAA-111"
    
    -- Filter and calculate average
    result <- Lib4.filterByPlateDSL "AAA-111"
    case result of
        Just v -> do
            avg <- Lib4.calculateAverageAgeDSL v
            putStrLnDSL $ "Average age for AAA-111: " ++ show avg
        Nothing -> putStrLnDSL "Vehicle not found"

-- Example 6: Dump examples
exampleDump :: Free Lib4.VehicleDSL [String]
exampleDump = do
    examples <- Lib4.dumpDSL
    mapM_ putStrLnDSL examples
    return examples

-- Helper function for printing (works with both interpreters)
putStrLnDSL :: String -> Free Lib4.VehicleDSL ()
putStrLnDSL _ = return () -- In real DSL, this would be part of the language

-- ============================================================================
-- HTTP Interpreter Example
-- ============================================================================

runHttpExample :: IO ()
runHttpExample = do
    putStrLn "\n=== Running with HTTP Interpreter ==="
    putStrLn "Connecting to server at http://localhost:3000/execute\n"
    
    let serverUrl = "POST http://localhost:3000/execute"
    
    putStrLn "Example 1: Add vehicle"
    Lib4.runHttpInterpreter serverUrl exampleAddVehicle
    putStrLn ""
    
    putStrLn "Example 2: Add and filter"
    Lib4.runHttpInterpreter serverUrl exampleAddAndFilter
    putStrLn ""
    
    putStrLn "Example 3: Add passenger"
    Lib4.runHttpInterpreter serverUrl exampleAddPassenger
    putStrLn ""
    
    putStrLn "Example 4: Calculate average"
    Lib4.runHttpInterpreter serverUrl exampleCalculateAverage
    putStrLn ""
    
    putStrLn "Example 5: Complex sequence"
    Lib4.runHttpInterpreter serverUrl exampleComplex
    putStrLn ""
    
    putStrLn "Example 6: Dump examples"
    examples <- Lib4.runHttpInterpreter serverUrl exampleDump
    mapM_ putStrLn examples
    putStrLn ""

-- ============================================================================
-- State Interpreter Example
-- ============================================================================

runStateExample :: IO ()
runStateExample = do
    putStrLn "\n=== Running with State Interpreter ==="
    putStrLn "Executing locally with State monad\n"
    
    let initialState = Lib3.emptyState
    
    putStrLn "Example 1: Add vehicle"
    let (result1, state1) = Lib4.runStateInterpreter initialState exampleAddVehicle
    putStrLn $ "State after: " ++ show (length $ Lib3.vehicles state1) ++ " vehicles"
    putStrLn ""
    
    putStrLn "Example 2: Add and filter"
    let (result2, state2) = Lib4.runStateInterpreter state1 exampleAddAndFilter
    putStrLn $ "State after: " ++ show (length $ Lib3.vehicles state2) ++ " vehicles"
    putStrLn ""
    
    putStrLn "Example 3: Add passenger"
    let (result3, state3) = Lib4.runStateInterpreter state2 exampleAddPassenger
    putStrLn $ "State after: " ++ show (length $ Lib3.vehicles state3) ++ " vehicles"
    putStrLn ""
    
    putStrLn "Example 4: Calculate average"
    let (result4, state4) = Lib4.runStateInterpreter state3 exampleCalculateAverage
    putStrLn $ "Result: " ++ show result4
    putStrLn ""
    
    putStrLn "Example 5: Complex sequence"
    let (result5, state5) = Lib4.runStateInterpreter state4 exampleComplex
    putStrLn $ "State after: " ++ show (length $ Lib3.vehicles state5) ++ " vehicles"
    putStrLn ""
    
    putStrLn "Example 6: Dump examples"
    let (examples, state6) = Lib4.runStateInterpreter state5 exampleDump
    mapM_ putStrLn examples
    putStrLn ""
    
    putStrLn $ "\nFinal state: " ++ show (length $ Lib3.vehicles state6) ++ " vehicles total"
