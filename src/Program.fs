module DefinitlyNotFriedChickenPlanner.Program

open System

open DefinitlyNotFriedChickenPlanner.RoomLayout.Generation
open DefinitlyNotFriedChickenPlanner.RoomLayout.Validation
open DefinitlyNotFriedChickenPlanner.RoomLayout.Scoring
open DefinitlyNotFriedChickenPlanner.RoomLayout.Printing
open DefinitlyNotFriedChickenPlanner.RoomLayout.Optimisation
open DefinitlyNotFriedChickenPlanner.GenerationConfig
open DefinitlyNotFriedChickenPlanner.GenerationConfig.Evolution
open DefinitlyNotFriedChickenPlanner.GenerationConfig.Generation
open DefinitlyNotFriedChickenPlanner.GenerationConfig.Mutation
open DefinitlyNotFriedChickenPlanner.GenerationConfig.Scoring

[<AutoOpen>]
module ProgramConfig =
    let room = 5, 5
    let configGenerations = 10
    let optimizationIterations = 100

    let simulationConfig = {
        calculationConfig = { room = room; roomsPerConfig = 100 }
        configsPerGeneration = 20
        keepPercentage = 0.3
        mutatePercentage = 0.6
        mutationConfig = { mutationStep = 5 }
    }

// Run code
[<EntryPoint>]
let main args =
    // Read the first arg as iteration count if provided, otherwise use default
    let defaultIterations = 1000

    let iterations =
        if args.Length > 0 then
            match System.Int32.TryParse(args.[0]) with
            | true, value -> value
            | _ -> defaultIterations
        else
            defaultIterations

    // Setup
    let startTime = DateTime.Now
    let seed = Random().Next()
    let random = Random seed

    // At first we use an evolutionary approach to find a fitting generation config
    let generationConfig =
        simulateGenerations random simulationConfig configGenerations |> Seq.head

    printfn "Generating appliances using seed %i for room size %A..." seed room
    printfn "Using generation config: %A" generationConfig

    // Generate room layouts and filter those, that are not valid
    let roomLayouts =
        seq { for _ in 1..iterations -> generateRoomLayout random room generationConfig }
        |> Seq.filter (fun layout -> validate room layout |> Result.isOk)
        |> Seq.cache

    let validRooms = Seq.length roomLayouts

    // Calculate tier 1 score
    let roomLayoutsWithScoreTier1 =
        roomLayouts
        |> Seq.map (fun layout -> layout, calculateScoreTier1 layout)
        |> Seq.cache

    let scoreTier1Max = roomLayoutsWithScoreTier1 |> Seq.map snd |> Seq.max

    let optimizedRoomLayoutsWithScoreTier2 =
        roomLayoutsWithScoreTier1
        // We only keep the room layout with the best tier 1 score
        |> Seq.filter (fun (_, scoreTier1) -> scoreTier1 = scoreTier1Max)
        |> Seq.map fst
        // We optimize the remaining room layouts for tier 2 score, but because optimization has a random component,
        // we run it multiple times to find the best configuration
        |> Seq.collect (fun appliances ->
            seq {
                for _ in 1..optimizationIterations do
                    yield optimiseEmitterCost random room appliances
            })
        // And we remove those, that are identical
        |> Seq.distinct
        // For those we calculate the tier 2 score
        |> Seq.map (fun appliances -> appliances, calculateScoreTier2 appliances)
        |> Seq.cache

    let scoreTier2Max = optimizedRoomLayoutsWithScoreTier2 |> Seq.map snd |> Seq.max

    let bestRoomLayouts =
        optimizedRoomLayoutsWithScoreTier2
        // We only keep the room layouts with the best tier 2 score
        |> Seq.filter (fun (_, scoreTier2) -> scoreTier2 = scoreTier2Max)
        |> Seq.map fst
        |> Seq.cache

    let endTime = DateTime.Now

    if Seq.isEmpty bestRoomLayouts then
        printfn "No valid appliance configuration found."
    else
        Seq.iter (printRoomLayout room) bestRoomLayouts

        printfn
            "Found %i valid configurations with %i growboxes and cost %.2f per hour"
            (Seq.length bestRoomLayouts)
            scoreTier1Max
            (scoreTier2Max * -1.)

    printfn "Total time taken: %A" (endTime - startTime)
    printfn "Room generation completed with %i (%i%%) valid rooms." validRooms (validRooms * 100 / iterations)

    0 // Return an integer exit code
