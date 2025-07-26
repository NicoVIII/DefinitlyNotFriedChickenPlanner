module DefinitlyNotFriedChickenPlanner.Generation

open System

open Validation

// Generation config
type GenerationConfig = {
    chanceForGrowbox: int
    chanceForEmitter: int
    chanceForOverhead: int
// TODO: min/max values for emitters
// TODO: optimization strategy (highest first, lowest first, outer first(?))
}

let generateGenerationConfig (random: Random) : GenerationConfig = {
    chanceForGrowbox = random.Next 21 * 5
    chanceForEmitter = random.Next 21 * 5
    chanceForOverhead = random.Next 21 * 5
}

// Helper for generation of rooms
let generateHeater (coord: Coordinate) =
    let appliance = {
        applianceType =
            Emitter {
                emitterType = Heater 70<Heat>
                overhead = false
            }
        coordinate = coord
    }

    appliance

let generateHumidifier (coord: Coordinate) =
    let appliance = {
        applianceType =
            Emitter {
                emitterType = Humidifier 75<Humidity>
                overhead = false
            }
        coordinate = coord
    }

    appliance

let generateSprinkler (coord: Coordinate) =
    let appliance = {
        applianceType =
            Emitter {
                emitterType = Sprinkler 80<Water>
                overhead = false
            }
        coordinate = coord
    }

    appliance

let generateGrowbox (random: Random) (coord: Coordinate) =
    let orientation =
        match random.Next 4 with
        | 0 -> North
        | 1 -> East
        | 2 -> South
        | _ -> West

    let adjCoord =
        match orientation with
        | North -> { x = coord.x; y = coord.y - 1 }
        | West -> { x = coord.x - 1; y = coord.y }
        | East -> { x = coord.x + 1; y = coord.y }
        | South -> { x = coord.x; y = coord.y + 1 }

    let appliance = {
        applianceType =
            Growbox {
                growboxType = Config.growboxTypes.cannabis
                orientation = orientation
            }
        coordinate = coord
    }

    appliance, adjCoord

let generateEmitter (random: Random) (coord: Coordinate) =
    match random.Next(0, 3) with
    | 0 -> generateHeater coord
    | 1 -> generateHumidifier coord
    | _ -> generateSprinkler coord

let generateGroundAppliances (random: Random) (config: GenerationConfig) (room: Room) : Appliance list =
    let mutable occupied = Set.empty<Coordinate>
    let appliances = ResizeArray<Appliance>()
    let isFree coord = not (Set.contains coord occupied)
    let roomCoords = Room.generateCoords room

    let addAppliance appliance =
        appliances.Add appliance
#if DEBUG
        // Debug: Check, if appliances are still valid
        match validateRoomLayout room (Set.ofSeq appliances) with
        | Ok() -> ()
        | Error error -> printfn "[DEBUG] Generated invalid room with error: %A" error
#endif
        occupied <- Set.add appliance.coordinate occupied

    // Generate growboxes
    for coord in roomCoords do
        if isFree coord && random.Next 100 < config.chanceForGrowbox then
            let appliance, adjCord = generateGrowbox random coord
            // It is possible to generate invalid growboxes, we simply ignore those for now
            if isFree adjCord && validateCoordinate room adjCord |> Result.isOk then
                addAppliance appliance
                occupied <- Set.add adjCord occupied

    // Generate emitters
    for coord in roomCoords do
        if isFree coord && random.Next 100 < config.chanceForEmitter then
            generateEmitter random coord |> addAppliance

    appliances |> Seq.toList

let generateOverheadLight () =
    Emitter {
        emitterType = Light 90<Light>
        overhead = true
    }

let generateOverheadAppliances (random: Random) (config: GenerationConfig) (room: Room) : Appliance list =
    let roomCoords = Room.generateCoords room
    let appliances = ResizeArray<Appliance>()

    // Generate some random overhead appliances for demonstration
    for coord in roomCoords do
        if random.Next 100 < config.chanceForOverhead then
            appliances.Add {
                applianceType = generateOverheadLight ()
                coordinate = coord
            }
#if DEBUG
            // Debug: Check, if appliances are still valid
            match validateRoomLayout room (Set.ofSeq appliances) with
            | Ok() -> ()
            | Error error -> printfn "[DEBUG] Generated invalid room with error: %A" error
#endif
    appliances |> Seq.toList

let generateRoomLayout (random: Random) (room: Room) (config: GenerationConfig) : RoomLayout =
    let roomLayout =
        [
            yield! generateGroundAppliances random config room
            yield! generateOverheadAppliances random config room
        ]
        |> Set.ofList
#if DEBUG
    // Debug: Check, if appliances are still valid
    match validateRoomLayout room roomLayout with
    | Ok() -> ()
    | Error error -> printfn "[DEBUG] Generated invalid room with error: %A" error
#endif
    roomLayout
