[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.RoomLayoutTypes

type Orientation =
    | North
    | East
    | South
    | West

type Emitter =
    | Heater of int8<Heat>
    | Humidifier of int8<Humidity>
    | Light of int8<Light>
    | Sprinkler of int8<Water>

type GrowboxType = {
    minMeasurements: Measurements
    maxMeasurements: Measurements
}

type Growbox = {
    growboxType: GrowboxType
    orientation: Orientation
}

type ApplianceType =
    | Emitter of Emitter
    | Growbox of Growbox

type Appliance = {
    applianceType: ApplianceType
    coordinate: Coordinate
}

type RoomLayout = Appliance list
