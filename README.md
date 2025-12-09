# fp-2025

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

## Lib1.hs

### BNF

```
<command> ::= "AddVehicle" <vehicle>
            | "FilterByPlate" <plate>
            | "AddPassenger" <plate> <sex> <age>
            | "Sequence" <command> <command>
            | "CalculateAverageAge" <vehicle>
            | "Dump" <dumpable>

<vehicle> ::= "Vehicle" <plate> <driver> <passengers>

<driver> ::= "Driver" <sex> <age>

<passengers> ::= "[" <passengerList> "]"

<passengerList> ::= <passenger> | <passenger> "," <passengerList>

<passenger> ::= "Passenger" <sex> <age>

<plate> ::= <string>
<sex> ::= 'M' | 'F'
<age> ::= <integer>

<dumpable> ::= "Examples"
```

# State Persistence

## Overview

The state persistence system converts the in-memory application state into a sequence of CLI commands that can be saved to a file and replayed to restore the exact same state. This approach ensures that the state file remains human-readable and compatible with the command-line interface.

## State Structure

The `State` data type contains:
```haskell
newtype State = State {
    vehicles :: [Lib1.Vehicle]
}
```

Each `Vehicle` contains:
- `vehiclePlate` - The vehicle's license plate (format: ABC-123)
- `driver` - Driver information (sex: 'M' or 'F', age: integer)
- `passengers` - List of passengers (each with sex and age)

## Mapping Strategy

### State Fields to Commands

1. **Vehicles List (`vehicles`)**: 
   - Each vehicle in the state is mapped to a single `add vehicle` command
   - All vehicle properties (driver, passengers) are included in the command
   
2. **Commands Not Persisted**:
   - `filter by plate` - Query command, doesn't modify state
   - `calculate average age` - Calculation command, doesn't modify state
   - `dump examples` - Display command, doesn't modify state
   - `add passenger` - When a passenger is added to a vehicle, it modifies the vehicle's passenger list, so the entire vehicle (with updated passengers) is saved in the next state persistence

## Examples

### Example 1: Simple State with Two Vehicles

**State:**
```haskell
State {
    vehicles = [
        Vehicle {
            vehiclePlate = "ABC-123",
            driver = Driver 'M' 35,
            passengers = []
        },
        Vehicle {
            vehiclePlate = "XYZ-789",
            driver = Driver 'F' 28,
            passengers = [Passenger 'M' 40]
        }
    ]
}
```

**Generated CLI Commands (state.txt):**
```
add vehicle ABC-123 M 35 []
add vehicle XYZ-789 F 28 [M 40]
```

### Example 2: Complex State with Multiple Passengers

**State:**
```haskell
State {
    vehicles = [
        Vehicle {
            vehiclePlate = "LMN-456",
            driver = Driver 'M' 50,
            passengers = [
                Passenger 'F' 22,
                Passenger 'M' 18
            ]
        },
        Vehicle {
            vehiclePlate = "DEF-321",
            driver = Driver 'F' 42,
            passengers = [
                Passenger 'M' 15,
                Passenger 'F' 12,
                Passenger 'M' 8
            ]
        }
    ]
}
```

**Generated CLI Commands (state.txt):**
```
add vehicle LMN-456 M 50 [F 22, M 18]
add vehicle DEF-321 F 42 [M 15, F 12, M 8]
```

## Persistence Demonstration

**Step 1: Initial Program Launch and Command Execution**
```
$ stack run fp2025-three
> add vehicle ABC-123 M 35 []
> add vehicle XYZ-789 F 28 [M 40]
> filter by plate ABC-123
Found vehicle: ABC-123 M 35 []
```

**Step 2: View State File**
```
$ cat state.txt
add vehicle ABC-123 M 35 []
add vehicle XYZ-789 F 28 [M 40]
```