[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.RoomLayout.Optimization

open System

open DefinitlyNotFriedChickenPlanner
open Validation

let optimizeEmitterCost strategy (room: Room) (roomLayout: RoomLayout) : RoomLayout =
    // We check, if the appliances are valid
    if validate room roomLayout |> Result.isError then
        failwith "Invalid appliances for optimisation"

    let appliances = roomLayout |> Set.toList

    // To simplify changing the emitters, we first collect them all via coordinates and overhead
    let mutable emitters =
        appliances
        |> List.choose (function
            | {
                  applianceType = Emitter emitterType
                  coordinate = coord
              } -> ((coord, emitterType.overhead), emitterType) |> Some
            | _ -> None)
        |> Map.ofList

    let rest =
        List.filter
            (function
            | { applianceType = Emitter _ } -> false
            | _ -> true)
            appliances

    let getRoomLayout emitterMap =
        rest
        @ (emitterMap
           |> Map.toList
           |> List.map (fun ((coord, _), emitterType) -> {
               applianceType = Emitter emitterType
               coordinate = coord
           }))
        |> Set.ofList

    let mutable remainingEmitters = emitters |> Map.keys |> List.ofSeq

    while not (List.isEmpty remainingEmitters) do
        let key =
            match strategy with
            | HighestFirst ->
                remainingEmitters
                |> List.maxBy (fun key -> Map.find key emitters |> Emitter.getValue)
            | LowestFirst ->
                remainingEmitters
                |> List.minBy (fun key -> Map.find key emitters |> Emitter.getValue)

        let reductionResult =
            Map.change
                key
                (Option.defaultWith (fun () -> failwith "Mapkey was missing")
                 >> (function
                 // TODO: Use lenses to make this code more readable
                 | { emitterType = Heater temp } as emitter when temp > 1<Heat> ->
                     Some {
                         emitter with
                             emitterType = Heater(temp - 1<Heat>)
                     }
                 | { emitterType = Humidifier hum } as emitter when hum > 1<Humidity> ->
                     Some {
                         emitter with
                             emitterType = Humidifier(hum - 1<Humidity>)
                     }
                 | { emitterType = Sprinkler water } as emitter when water > 1<Water> ->
                     Some {
                         emitter with
                             emitterType = Sprinkler(water - 1<Water>)
                     }
                 | { emitterType = Light light } as emitter when light > 1<Light> ->
                     Some {
                         emitter with
                             emitterType = Light(light - 1<Light>)
                     }
                 // We remove the emitter, if we would reduce its value to 0
                 | { emitterType = Heater _ }
                 | { emitterType = Humidifier _ }
                 | { emitterType = Sprinkler _ }
                 | { emitterType = Light _ } -> None))
                emitters

        if getRoomLayout reductionResult |> validate room |> Result.isOk then
            // We could make a valid reduction, we therefore save it
            emitters <- reductionResult
            // And we allow all emitters to be chosen again
            remainingEmitters <- emitters |> Map.keys |> List.ofSeq
        else
            // No success with this emitter, we therefore remove it from the list of possibilities
            remainingEmitters <- remainingEmitters |> List.filter ((<>) key)

    getRoomLayout emitters
