[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.RoomLayout.Validation

open FsToolkit.ErrorHandling
open Microsoft.FSharp.Core.Operators.Checked
open SimpleOptics

open DefinitlyNotFriedChickenPlanner
open DefinitlyNotFriedChickenPlanner.Helper

// Types
type PlacementValidationError =
    | DoublePlacement
    | InvalidCoordinate
    | InvalidOverheadSetting
    | GrowboxOutOfBounds
    | GrowboxAdjacentTileOccupied
    | GrowboxAdjacentTileNeeded

type GrowboxNeedsNotMetError =
    | InvalidHeat of Coordinate * int8<Heat>
    | InvalidHumidity of Coordinate * int8<Humidity>
    | InvalidLight of Coordinate * int8<Light>
    | InvalidWater of Coordinate * int8<Water>

type ValidationError =
    | Placement of PlacementValidationError
    | GrowboxNeedsNotMet of GrowboxNeedsNotMetError list

// Helper objects
let defaultMeasurements = {
    heat = 0y<Heat>
    humidity = 0y<Humidity>
    light = 0y<Light>
    water = 0y<Water>
}

// Functions
let validateCoordinate room (coordinate: Coordinate) =
    result {
        if
            coordinate.x < 0uy
            || coordinate.x >= room.width
            || coordinate.y < 0uy
            || coordinate.y >= room.height
        then
            return! Error InvalidCoordinate
    }

let validateApplianceOverhead appliance =
    result {
        // Check if the overhead setting is valid for the appliance type
        // Lights have be overhead, all other appliances must be on the ground
        match appliance.applianceType, appliance.coordinate.overhead with
        | Growbox _, false
        | Emitter(Heater _), false
        | Emitter(Humidifier _), false
        | Emitter(Sprinkler _), false
        | Emitter(Lamp _), true -> ()
        | _ -> return! Error InvalidOverheadSetting
    }

let getNextCoordinate orientation coord : Coordinate =
    match orientation with
    | North -> { coord with y = coord.y - 1uy }
    | South -> { coord with y = coord.y + 1uy }
    | East -> { coord with x = coord.x + 1uy }
    | West -> { coord with x = coord.x - 1uy }

let validateRoomLayout (room: Room) roomLayout : Result<unit, PlacementValidationError> =
    let checkForDoublePlacement roomLayout =
        result {
            let distinct = List.distinctBy (fun a -> a.coordinate) roomLayout

            if List.length distinct <> List.length roomLayout then
                return! Error DoublePlacement
        }

    let isOutOfBounds orientation coord =
        match orientation with
        | North -> coord.y <= 0uy
        | South -> coord.y >= room.height - 1uy
        | East -> coord.x >= room.width - 1uy
        | West -> coord.x <= 0uy

    result {
        // Check if all appliances have valid overhead settings
        do! List.traverseResultM validateApplianceOverhead roomLayout |> Result.ignore

        let groundAppliances, overheadAppliances =
            List.partition (_.coordinate >> _.overhead) roomLayout

        // Check for double placement on the ground and overhead
        do! checkForDoublePlacement groundAppliances
        do! checkForDoublePlacement overheadAppliances

        // Check if all appliances are within the room bounds
        do!
            roomLayout
            |> List.map (fun a -> a.coordinate)
            |> List.traverseResultM (validateCoordinate room)
            |> Result.ignore

        // Check if all growboxes have enough space
        do!
            roomLayout
            |> List.choose (function
                | {
                      applianceType = Growbox growbox
                      coordinate = coordinate
                  } -> (growbox, coordinate) |> Some
                | _ -> None)
            |> List.fold
                (fun neededCoordinatesResult (growbox, coordinate) ->
                    Result.bind
                        (fun (neededCoordinates: Coordinate list) ->
                            let orientation = growbox.orientation

                            result {
                                if isOutOfBounds orientation coordinate then
                                    return! Error GrowboxOutOfBounds

                                let nextCoordinate = getNextCoordinate orientation coordinate

                                if List.exists (_.coordinate >> (=) nextCoordinate) groundAppliances then
                                    return! Error GrowboxAdjacentTileOccupied

                                if List.exists ((=) nextCoordinate) neededCoordinates then
                                    return! Error GrowboxAdjacentTileNeeded

                                return nextCoordinate :: neededCoordinates
                            })
                        neededCoordinatesResult)
                (Ok [])
            |> Result.ignore
    }

let calculateTileDistance (a: Coordinate) (b: Coordinate) : uint8<Tile> =
    let xDiff = if a.x > b.x then a.x - b.x else b.x - a.x
    let yDiff = if a.y > b.y then a.y - b.y else b.y - a.y
    xDiff + yDiff |> (*) 1uy<Tile>

let calculateMeasure
    room
    roomLayout
    (measurePrism: Prism<Appliance, int8<'a>>)
    (maxMeasure: int16<'a>)
    (reduction: int16<'a / Tile>)
    =
    let measurementMap =
        let width = room.width |> int
        let height = room.height |> int
        Array2D.create<int8<'a>> width height 0y<_>

    let emitters =
        roomLayout
        |> List.choose (fun appliance ->
            Optic.get measurePrism appliance
            |> Option.map (fun heater -> heater, appliance.coordinate))

    for emitterValue, emitterCoord in emitters do
        for coord in Room.generateCoords false room do
            let x, y = int coord.x, int coord.y
            let distance = calculateTileDistance emitterCoord coord |> uint8Toint16
            let adjustment = int8ToInt16 emitterValue - distance * reduction |> max 0s<_>
            measurementMap.[x, y] <- int8ToInt16 measurementMap.[x, y] + adjustment |> min maxMeasure |> int16ToInt8

    measurementMap

let calculateMeasurements (room: Room) roomLayout =
    let measurementMap =
        let width = room.width |> int
        let height = room.height |> int
        Array2D.create<Measurements> width height defaultMeasurements

    let emitters =
        roomLayout
        |> List.choose (function
            | {
                  applianceType = Emitter emitter
                  coordinate = coordinate
              } -> Some(emitter, coordinate)
            | _ -> None)

    for emitter, emitterCoord in emitters do
        for coord in Room.generateCoords false room do
            let x, y = int coord.x, int coord.y
            let distance = calculateTileDistance emitterCoord coord |> uint8Toint16

            match emitter with
            | Heater intensity ->
                let adjustment =
                    int8ToInt16 intensity - distance * Config.heat.reduction |> max 0s<Heat>

                measurementMap.[x, y] <- {
                    measurementMap.[x, y] with
                        heat =
                            int8ToInt16 measurementMap.[x, y].heat + adjustment
                            |> min Config.heat.max
                            |> int16ToInt8
                }
            | Humidifier intensity ->
                let adjustment =
                    int8ToInt16 intensity - distance * Config.humidity.reduction |> max 0s<Humidity>

                measurementMap.[x, y] <- {
                    measurementMap.[x, y] with
                        humidity =
                            int8ToInt16 measurementMap.[x, y].humidity + adjustment
                            |> min 100s<Humidity>
                            |> int16ToInt8
                }
            | Lamp intensity ->
                let adjustment =
                    int8ToInt16 intensity - distance * Config.light.reduction |> max 0s<Light>

                measurementMap.[x, y] <- {
                    measurementMap.[x, y] with
                        light =
                            int8ToInt16 measurementMap.[x, y].light + adjustment
                            |> min 100s<Light>
                            |> int16ToInt8
                }
            | Sprinkler intensity ->
                let adjustment =
                    int8ToInt16 intensity - distance * Config.water.reduction |> max 0s<Water>

                measurementMap.[x, y] <- {
                    measurementMap.[x, y] with
                        water =
                            int8ToInt16 measurementMap.[x, y].water + adjustment
                            |> min 100s<Water>
                            |> int16ToInt8
                }

    measurementMap

let validateGrowboxNeedsForMeasure roomLayout getValue error (measurements: int8<'a>[,]) =
    let checkGrowbox (growbox: Growbox) (coordinate: Coordinate) =
        result {
            let minMeasurement = getValue growbox.growboxType.minMeasurements
            let maxMeasurement = getValue growbox.growboxType.maxMeasurements
            let measurement = measurements.[int coordinate.x, int coordinate.y]

            if measurement < minMeasurement || measurement > maxMeasurement then
                return! error (coordinate, measurement) |> Error
        }

    roomLayout
    |> List.choose (function
        | {
              applianceType = Growbox growbox
              coordinate = coordinate
          } -> Some(growbox, coordinate)
        | _ -> None)
    |> List.traverseResultA (fun (growbox, coordinate) -> checkGrowbox growbox coordinate)
    |> Result.ignore

let validateGrowboxNeeds roomLayout (measurements: Measurements[,]) =
    let checkGrowbox (growbox: Growbox) (coordinate: Coordinate) =
        result {
            let minMeasurements = growbox.growboxType.minMeasurements
            let maxMeasurements = growbox.growboxType.maxMeasurements
            let measurement = measurements.[int coordinate.x, int coordinate.y]

            if
                measurement.heat < minMeasurements.heat
                || measurement.heat > maxMeasurements.heat
            then
                return! InvalidHeat(coordinate, measurement.heat) |> Error

            if
                measurement.humidity < minMeasurements.humidity
                || measurement.humidity > maxMeasurements.humidity
            then
                return! InvalidHumidity(coordinate, measurement.humidity) |> Error

            if
                measurement.light < minMeasurements.light
                || measurement.light > maxMeasurements.light
            then
                return! InvalidLight(coordinate, measurement.light) |> Error

            if
                measurement.water < minMeasurements.water
                || measurement.water > maxMeasurements.water
            then
                return! InvalidWater(coordinate, measurement.water) |> Error
        }

    roomLayout
    |> List.choose (function
        | {
              applianceType = Growbox growbox
              coordinate = coordinate
          } -> Some(growbox, coordinate)
        | _ -> None)
    |> List.traverseResultA (fun (growbox, coordinate) -> checkGrowbox growbox coordinate)
    |> Result.ignore

let validateMeasure measure room roomLayout =
    match measure with
    | Heat ->
        let measurePrism = ApplianceOptic.heater
        let maxMeasure = Config.heat.max
        let reduction = Config.heat.reduction
        let getValue = fun measurement -> measurement.heat

        calculateMeasure room roomLayout measurePrism maxMeasure reduction
        |> validateGrowboxNeedsForMeasure roomLayout getValue InvalidHeat
    | Humidity ->
        let measurePrism = ApplianceOptic.humidifier
        let maxMeasure = Config.humidity.max
        let reduction = Config.humidity.reduction
        let getValue = fun measurement -> measurement.humidity

        calculateMeasure room roomLayout measurePrism maxMeasure reduction
        |> validateGrowboxNeedsForMeasure roomLayout getValue InvalidHumidity
    | Measure.Light ->
        let measurePrism = ApplianceOptic.light
        let maxMeasure = Config.light.max
        let reduction = Config.light.reduction
        let getValue = fun measurement -> measurement.light

        calculateMeasure room roomLayout measurePrism maxMeasure reduction
        |> validateGrowboxNeedsForMeasure roomLayout getValue InvalidLight
    | Water ->
        let measurePrism = ApplianceOptic.sprinkler
        let maxMeasure = Config.water.max
        let reduction = Config.water.reduction
        let getValue = fun measurement -> measurement.water

        calculateMeasure room roomLayout measurePrism maxMeasure reduction
        |> validateGrowboxNeedsForMeasure roomLayout getValue InvalidWater

let validateMeasures room roomLayout =
    calculateMeasurements room roomLayout |> validateGrowboxNeeds roomLayout

let validate room (roomLayout: RoomLayout) =
    result {
        do! validateRoomLayout room roomLayout |> Result.mapError Placement
        do! validateMeasures room roomLayout |> Result.mapError GrowboxNeedsNotMet
    }
