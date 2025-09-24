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

``
<command> ::= "AddVehicle" <vehicle>
            | "FilterByPlate" <plate>
            | "AddPassenger" <plate> <sex> <age>
            | "Sequence" <command> <command>
            | "CalculateAverageAge" <vehicle>
            | "Dump" <dumpable>
``
