# fp-2025

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

## Lib1.hs

### BNFs

``
- AddVehicle (Vehicle {vehiclePlate = "ABC-123", driver = Driver {driverSex = "M", driverAge = 35}, passengers = []})
``

``
- FilterByPlate "LMN-456"
``

``
- AddPassenger "ABC-123" "F" 19
``

``
- CalculateAverageAge (Vehicle {vehiclePlate = "LMN-456", driver = Driver {driverSex = "M", driverAge = 50}, passengers = [Passenger {passengerSex = "F", passengerAge = 22}]})
``

``
- Sequence (Sequence (AddVehicle (Vehicle {vehiclePlate = "ABC-123", driver = Driver {driverSex = "M", driverAge = 35}, passengers = []})) (AddVehicle (Vehicle {vehiclePlate = "XYZ-789", driver = Driver {driverSex = "F", driverAge = 28}, passengers = [Passenger {passengerSex = "M", passengerAge = 40},Passenger {passengerSex = "F", passengerAge = 25}]}))) (AddVehicle (Vehicle {vehiclePlate = "LMN-456", driver = Driver {driverSex = "M", driverAge = 50}, passengers = [Passenger {passengerSex = "F", passengerAge = 22}]}))
``
