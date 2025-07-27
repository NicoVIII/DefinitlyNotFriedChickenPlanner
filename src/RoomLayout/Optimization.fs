[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.RoomLayout.Optimization

open Microsoft.FSharp.Core.Operators.Checked
open SimpleOptics
open System
open System.Collections.Generic

open DefinitlyNotFriedChickenPlanner
open Validation

let reduceEmitter appliance =
    Optic.get ApplianceOptic.emitterValue appliance
    |> Option.defaultWith (fun () -> failwith "Expected Ok, but got Error")
    |> function
        | value when value > 1y -> Optic.set ApplianceOptic.emitterValue (value - 1y) appliance |> Some
        | _ -> None

let optimizeEmitterCost strategy (room: Room) (roomLayout: RoomLayout) : RoomLayout =
    // We check, if the appliances are valid
    match validate room roomLayout with
    | Ok _ -> ()
    | Error error -> failwithf "Invalid appliances for optimisation: %A" error

    let coordinateToKey coord : uint =
        let xBits = uint coord.x
        let yBits = uint coord.y <<< 8
        let overheadBit = if coord.overhead then 1u <<< 16 else 0u
        xBits + yBits + overheadBit

    let appliances = roomLayout |> Set.toList

    let emitterMapData =
        appliances
        |> List.choose (function
            | { applianceType = Emitter _ } as emitter ->
                (coordinateToKey emitter.coordinate, emitter) |> KeyValuePair |> Some
            | _ -> None)

    // We track the changes in two maps - one contains always a valid state
    let validEmitterMap = new Dictionary<uint, Appliance>(emitterMapData)
    let experimentalEmitterMap = new Dictionary<uint, Appliance>(validEmitterMap)

    let rest =
        appliances
        |> List.filter (function
            | { applianceType = Emitter _ } -> false
            | _ -> true)

    let getRoomLayout (emitterMap: Dictionary<uint, Appliance>) =
        [
            for key in emitterMap.Keys do
                emitterMap.[key]
        ]
        |> List.append rest

    let performChange (key: uint) (map: Dictionary<uint, Appliance>) =
        match reduceEmitter map.[key] with
        | Some reducedEmitter -> map.[key] <- reducedEmitter
        | None -> map.Remove key |> ignore

    let rec optimize remainingKeys =
        match remainingKeys with
        | [] -> ()
        | _ ->
            let key =
                match remainingKeys, strategy with
                | [ key ], _ -> key // Only one key left, we have to use it
                | _, HighestFirst ->
                    remainingKeys
                    |> List.maxBy (fun key ->
                        validEmitterMap.[key]
                        |> Optic.get ApplianceOptic.emitterValue
                        |> Option.defaultWith (fun () -> failwithf "Expected Ok, but got Error"))
                | _, LowestFirst ->
                    remainingKeys
                    |> List.minBy (fun key ->
                        validEmitterMap.[key]
                        |> Optic.get ApplianceOptic.emitterValue
                        |> Option.defaultWith (fun () -> failwithf "Expected Ok, but got Error"))

            // We perform the change on the experimental map
            performChange key experimentalEmitterMap

            // We check if the change is valid
            if getRoomLayout experimentalEmitterMap |> validateList room |> Result.isOk then
                let remainingKeys =
                    match strategy with
                    | HighestFirst ->
                        // Restart with all keys
                        experimentalEmitterMap.Keys |> Seq.toList
                    | LowestFirst ->
                        // If we are in LowestFirst mode, we potentially know the lowest key
                        // If it is still in the map, it is the current one
                        if experimentalEmitterMap.ContainsKey key then
                            [ key ]
                        else
                            // If it is not in the map, we reset the remaining keys
                            experimentalEmitterMap.Keys |> Seq.toList

                // Valid reduction, save it
                performChange key validEmitterMap
                optimize remainingKeys
            else
                // Reset experimental dict, remove this key and try others
                experimentalEmitterMap.[key] <- validEmitterMap.[key]
                optimize (List.filter ((<>) key) remainingKeys)

    optimize (validEmitterMap.Keys |> Seq.toList)
    getRoomLayout validEmitterMap |> Set.ofList
