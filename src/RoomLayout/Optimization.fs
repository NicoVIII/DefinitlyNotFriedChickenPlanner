[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.RoomLayout.Optimization

open Microsoft.FSharp.Core.Operators.Checked
open SimpleOptics

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

    let optimizeMeasure measure chooser roomLayout =
        let emitterMapData = List.choose chooser roomLayout
        let rest = List.filter (chooser >> Option.isNone) roomLayout

        let getRoomLayout (emitterMap: Map<Coordinate, Appliance>) =
            Map.values emitterMap |> List.ofSeq |> List.append rest

        let performChange (key: Coordinate) (map: Map<Coordinate, Appliance>) =
            Map.change key (Option.bind reduceEmitter) map

        // We track the changes in two maps - one contains always a valid state
        let rec optimize remainingKeys valueMap validEmitterMap =
            match remainingKeys with
            | [] -> validEmitterMap
            | _ ->
                let key =
                    match remainingKeys, strategy with
                    | [ key ], _ -> key // Only one key left, we have to use it
                    | _, HighestFirst -> remainingKeys |> List.maxBy (fun key -> Map.find key valueMap)
                    | _, LowestFirst -> remainingKeys |> List.minBy (fun key -> Map.find key valueMap)

                // We perform the change on the experimental map
                let experimentalEmitterMap = performChange key validEmitterMap

                // We check if the change is valid
                if
                    getRoomLayout experimentalEmitterMap
                    |> validateMeasure measure room
                    |> Result.isOk
                then
                    // Valid reduction, save it and use all remaining keys
                    let remainingKeys =
                        if Map.count experimentalEmitterMap < Map.count validEmitterMap then
                            // If we removed the current key, we can remove it from the remaining keys
                            List.filter ((<>) key) remainingKeys
                        else
                            remainingKeys

                    // We update the value map as well
                    let updatedValueMap = Map.change key (Option.map ((-) 1y)) valueMap

                    experimentalEmitterMap |> optimize remainingKeys updatedValueMap
                else
                    // Remove this key and try others
                    optimize (List.filter ((<>) key) remainingKeys) valueMap validEmitterMap

        let emitterMap = Map.ofList emitterMapData
        // We track the pure values in another map to speed up the lookup
        let valueMap =
            Map.map
                (fun _ ->
                    Optic.get ApplianceOptic.emitterValue
                    >> Option.defaultWith (fun () -> failwithf "Expected ok"))
                emitterMap

        optimize (Map.keys emitterMap |> Seq.toList) valueMap emitterMap
        |> getRoomLayout

    // We optimize one measure after the other
    roomLayout
    |> optimizeMeasure Heat (function
        | { applianceType = Emitter(Heater _) } as a -> Some(a.coordinate, a)
        | _ -> None)
    |> optimizeMeasure Humidity (function
        | {
              applianceType = Emitter(Humidifier _)
          } as a -> Some(a.coordinate, a)
        | _ -> None)
    |> optimizeMeasure Light (function
        | { applianceType = Emitter(Lamp _) } as a -> Some(a.coordinate, a)
        | _ -> None)
    |> optimizeMeasure Water (function
        | { applianceType = Emitter(Sprinkler _) } as a -> Some(a.coordinate, a)
        | _ -> None)
