[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.RoomLayout.Generation

open Microsoft.FSharp.Core.Operators.Checked
open System

open DefinitlyNotFriedChickenPlanner
open Validation

// Helper for generation of rooms
let buildHeater (coord: Coordinate) = {
    applianceType = Emitter(Heater 70y<Heat>)
    coordinate = coord
}

let buildHumidifier (coord: Coordinate) = {
    applianceType = Emitter(Humidifier 75y<Humidity>)
    coordinate = coord
}

let buildSprinkler (coord: Coordinate) = {
    applianceType = Emitter(Sprinkler 80y<Water>)
    coordinate = coord
}

let buildLight (coord: Coordinate) = {
    applianceType = Emitter(Lamp 90y<Light>)
    coordinate = coord
}

let generateOrientation (random: Random) =
    match random.Next 4 with
    | 0 -> North
    | 1 -> West
    | 2 -> East
    | _ -> South

let buildGrowbox (orientation: Orientation) (coord: Coordinate) = {
    applianceType =
        Growbox {
            growboxType = Config.growboxTypes.cannabis
            orientation = orientation
        }
    coordinate = coord
}

let generateGroundAppliances (random: Random) (config: GenerationConfig) (room: Room) : Appliance list =
    let mutable occupied = Set.empty<Coordinate>
    let appliances = ResizeArray<Appliance>()
    let isFree coord = not (Set.contains coord occupied)
    let roomCoords = Room.generateCoords false room

    let addAppliance appliance =
        appliances.Add appliance
#if DEBUG
        // Debug: Check, if appliances are still valid
        match validateRoomLayout room (List.ofSeq appliances) with
        | Ok() -> ()
        | Error error -> printfn "[DEBUG] Generated invalid room with error: %A" error
#endif
        occupied <- Set.add appliance.coordinate occupied

    // Generate growboxes
    for coord in roomCoords do
        if isFree coord && random.Next 100 < config.chances.forGrowbox then
            let orientation = generateOrientation random
            let adjCoord = getNextCoordinate orientation coord

            let appliance = buildGrowbox orientation coord
            // It is possible to generate invalid growboxes, we simply ignore those for now
            if isFree adjCoord && validateCoordinate room adjCoord |> Result.isOk then
                addAppliance appliance
                occupied <- Set.add adjCoord occupied

    // Generate emitters
    for coord in roomCoords do
        if isFree coord then
            if random.Next 100 < config.chances.forSprinkler then
                buildSprinkler coord |> addAppliance
            elif random.Next 100 < config.chances.forHeater then
                buildHeater coord |> addAppliance
            elif random.Next 100 < config.chances.forHumidifier then
                buildHumidifier coord |> addAppliance

    appliances |> Seq.toList

let generateOverheadAppliances (random: Random) (config: GenerationConfig) (room: Room) : Appliance list =
    let roomCoords = Room.generateCoords true room
    let appliances = ResizeArray<Appliance>()

    // Generate some random overhead appliances for demonstration
    for coord in roomCoords do
        if random.Next 100 < config.chances.forLight then
            buildLight coord |> appliances.Add
#if DEBUG
            // Debug: Check, if appliances are still valid
            match validateRoomLayout room (List.ofSeq appliances) with
            | Ok() -> ()
            | Error error -> printfn "[DEBUG] Generated invalid room with error: %A" error
#endif
    appliances |> Seq.toList

let generateRoomLayout (random: Random) (room: Room) (config: GenerationConfig) : RoomLayout =
    let roomLayout = [
        yield! generateGroundAppliances random config room
        yield! generateOverheadAppliances random config room
    ]
#if DEBUG
    // Debug: Check, if appliances are still valid
    match validateRoomLayout room roomLayout with
    | Ok() -> ()
    | Error error -> printfn "[DEBUG] Generated invalid room with error: %A" error
#endif
    roomLayout
