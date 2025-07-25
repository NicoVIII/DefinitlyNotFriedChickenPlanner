module DefinitlyNotFriedChickenPlanner.Optimisation

open System
open Validation

let optimiseEmitterCost (random: Random) (room: Room) (roomLayout: RoomLayout) : RoomLayout =
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
        let index = List.length remainingEmitters |> random.Next
        let key = remainingEmitters |> List.item index

        // We tune down the value until this is no longer valid
        let mutable newEmitters = emitters
        let mutable couldReduce = false
        let mutable isValid = true
        let mutable removedEmitter = false

        let reduce () =
            Map.change
                key
                (Option.defaultWith (fun () -> failwith "Mapkey was missing")
                 >> (function
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
                 | { emitterType = Light _ } ->
                     removedEmitter <- true
                     None))
                newEmitters

        // We reduce the emitter until it is no longer valid or we removed it
        while isValid && not removedEmitter do
            newEmitters <- reduce ()
            isValid <- getRoomLayout newEmitters |> validate room |> Result.isOk

            if isValid then
                emitters <- newEmitters
                couldReduce <- true

        // If we could reduce, we restart the list
        if couldReduce then
            remainingEmitters <- emitters |> Map.keys |> List.ofSeq

        // And now we remove the current key from the list
        remainingEmitters <- remainingEmitters |> List.filter ((<>) key)

    getRoomLayout emitters
