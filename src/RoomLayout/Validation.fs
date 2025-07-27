module DefinitlyNotFriedChickenPlanner.RoomLayout.Validation

open FsToolkit.ErrorHandling

open DefinitlyNotFriedChickenPlanner

// Types
type PlacementValidationError =
    | DoublePlacement
    | InvalidCoordinate
    | GrowboxOutOfBounds
    | GrowboxAdjacentTileOccupied
    | GrowboxAdjacentTileNeeded

type ValidationError =
    | Placement of PlacementValidationError
    | Unknown

// Helper objects
let defaultMeasurements = {
    heat = 0<Heat>
    humidity = 0<Humidity>
    light = 0<Light>
    water = 0<Water>
}

// Functions
let validateCoordinate room (coordinate: Coordinate) =
    let width, height = room

    result {
        if
            coordinate.x < 0
            || coordinate.x >= width
            || coordinate.y < 0
            || coordinate.y >= height
        then
            return! Error InvalidCoordinate
    }

let validateRoomLayout (room: Room) (roomLayout: RoomLayout) : Result<unit, PlacementValidationError> =
    let width, height = room
    let appliances = Set.toList roomLayout

    let checkForDoublePlacement appliances =
        result {
            let distinct = List.distinctBy (fun a -> a.coordinate) appliances

            if List.length distinct <> List.length appliances then
                return! Error DoublePlacement
        }

    let getNextCoordinate orientation coord =
        match orientation with
        | North -> { coord with y = coord.y - 1 }
        | South -> { coord with y = coord.y + 1 }
        | East -> { coord with x = coord.x + 1 }
        | West -> { coord with x = coord.x - 1 }

    let isOutOfBounds orientation coord =
        match orientation with
        | North -> coord.y <= 0
        | South -> coord.y >= height - 1
        | East -> coord.x >= width - 1
        | West -> coord.x <= 0

    result {
        let groundAppliances, overheadAppliances =
            List.partition
                (function
                | {
                      applianceType = Emitter { overhead = true }
                  } -> false
                | _ -> true)
                appliances

        // Check for double placement on the ground and overhead
        do! checkForDoublePlacement groundAppliances
        do! checkForDoublePlacement overheadAppliances

        // Check if all appliances are within the room bounds
        do!
            appliances
            |> List.map (fun a -> a.coordinate)
            |> List.traverseResultM (validateCoordinate room)
            |> Result.ignore

        // Check if all growboxes have enough space
        do!
            appliances
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

let calculateTileDistance (a: Coordinate) (b: Coordinate) : int<Tile> =
    abs (a.x - b.x) + abs (a.y - b.y) |> (*) 1<Tile>

let calculateMeasurements (room: Room) (roomLayout: RoomLayout) =
    let clipToBounds measurements = {
        heat = measurements.heat |> max -100<Heat> |> min 100<Heat>
        humidity = measurements.humidity |> max -100<Humidity> |> min 100<Humidity>
        light = measurements.light |> max 0<Light> |> min 100<Light>
        water = measurements.water |> max 0<Water> |> min 100<Water>
    }

    let measurementMap =
        Array2D.create<Measurements> (fst room) (snd room) defaultMeasurements

    let emitters =
        roomLayout
        |> Set.toList
        |> List.choose (function
            | {
                  applianceType = Emitter emitter
                  coordinate = coordinate
              } -> Some(emitter, coordinate)
            | _ -> None)

    for emitter, coordinate in emitters do
        for x in 0 .. (fst room - 1) do
            for y in 0 .. (snd room - 1) do
                let distance = calculateTileDistance coordinate { x = x; y = y }

                match emitter.emitterType with
                | Heater intensity ->
                    let adjustment = max 0<Heat> (intensity - distance * Config.heat.reduction)

                    measurementMap.[x, y] <- {
                        measurementMap.[x, y] with
                            heat = measurementMap.[x, y].heat + adjustment
                    }
                | Humidifier intensity ->
                    let adjustment = max 0<Humidity> (intensity - distance * Config.humidity.reduction)

                    measurementMap.[x, y] <- {
                        measurementMap.[x, y] with
                            humidity = measurementMap.[x, y].humidity + adjustment
                    }
                | Light intensity ->
                    let adjustment = max 0<Light> (intensity - distance * Config.light.reduction)

                    measurementMap.[x, y] <- {
                        measurementMap.[x, y] with
                            light = measurementMap.[x, y].light + adjustment
                    }
                | Sprinkler intensity ->
                    let adjustment = max 0<Water> (intensity - distance * Config.water.reduction)

                    measurementMap.[x, y] <- {
                        measurementMap.[x, y] with
                            water = measurementMap.[x, y].water + adjustment
                    }

    measurementMap |> Array2D.map clipToBounds

let validateGrowboxNeeds (roomLayout: RoomLayout) (measurements: Measurements[,]) =
    let checkGrowbox (growbox: Growbox) (coordinate: Coordinate) =
        let minMeasurements = growbox.growboxType.minMeasurements
        let maxMeasurements = growbox.growboxType.maxMeasurements
        let measurement = measurements.[coordinate.x, coordinate.y]

        if
            measurement.heat < minMeasurements.heat
            || measurement.heat > maxMeasurements.heat
        then
            failwithf "Growbox at %A has invalid heat %A." coordinate measurement.heat

        if
            measurement.humidity < minMeasurements.humidity
            || measurement.humidity > maxMeasurements.humidity
        then
            failwithf "Growbox at %A has invalid humidity %A." coordinate measurement.humidity

        if
            measurement.light < minMeasurements.light
            || measurement.light > maxMeasurements.light
        then
            failwithf "Growbox at %A has invalid light %A." coordinate measurement.light

        if
            measurement.water < minMeasurements.water
            || measurement.water > maxMeasurements.water
        then
            failwithf "Growbox at %A has invalid water %A." coordinate measurement.water

    roomLayout
    |> Set.toList
    |> List.choose (function
        | {
              applianceType = Growbox growbox
              coordinate = coordinate
          } -> Some(growbox, coordinate)
        | _ -> None)
    |> List.iter (fun (growbox, coordinate) -> checkGrowbox growbox coordinate)

let validate room roomLayout =
    try
        result {
            do! validateRoomLayout room roomLayout |> Result.mapError Placement
            let measurements = calculateMeasurements room roomLayout
            validateGrowboxNeeds roomLayout measurements
        }
    with _ ->
        Error Unknown
