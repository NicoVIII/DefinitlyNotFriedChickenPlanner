[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.RoomLayoutTypes

type Orientation =
    | North
    | East
    | South
    | West

type EmitterType =
    | Heater of int<Heat>
    | Humidifier of int<Humidity>
    | Light of int<Light>
    | Sprinkler of int<Water>

type Emitter = {
    emitterType: EmitterType
    overhead: bool
}

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

type RoomLayout = Set<Appliance>
