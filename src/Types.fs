namespace DefinitlyNotFriedChickenPlanner

open Microsoft.FSharp.Core.LanguagePrimitives

// Units
[<Measure>]
type Heat

[<Measure>]
type Humidity

[<Measure>]
type Light

[<Measure>]
type Water

[<Measure>]
type Tile

[<Measure>]
type Dollar

[<Measure>]
type Hour

[<Measure>]
type ScoreTier1

[<Measure>]
type ScoreTier2

// Types
type Coordinate = { x: int; y: int }

type Orientation =
    | North
    | East
    | South
    | West

type Measurements = {
    heat: int<Heat>
    humidity: int<Humidity>
    light: int<Light>
    water: int<Water>
}

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

type Room = int * int

type CalculationData = Room * Appliance list
