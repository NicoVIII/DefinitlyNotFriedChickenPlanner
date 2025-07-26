module DefinitlyNotFriedChickenPlanner.Program

open System

open Generation
open Validation
open Scoring
open Printing
open Optimisation

[<AutoOpen>]
module Config =
    let room = 5, 5
    let configGenerations = 10
    let configsPerGeneration = 20
    let roomsPerConfig = 100
    let keepPercentage = 0.3
    let mutatePercentage = keepPercentage * 2.
    let randomPercentage = 1. - keepPercentage - mutatePercentage
    let mutationStep = 5
    let optimizationIterations = 100

let generateGenerationConfigs (random: Random) x =
    seq {
        for _ in 1..x do
            yield generateGenerationConfig random
    }

let mutateGenerationConfig (random: Random) (config: GenerationConfig) =
    let factor = if random.Next 2 = 1 then 1 else -1
    let mutation = factor * mutationStep

    match random.Next 3 with
    | 0 -> {
        config with
            chanceForGrowbox = config.chanceForGrowbox + mutation
      }
    | 1 -> {
        config with
            chanceForEmitter = config.chanceForEmitter + mutation
      }
    | _ -> {
        config with
            chanceForOverhead = config.chanceForOverhead + mutation
      }

let calculateGeneratorConfigScore (random: Random) (config: GenerationConfig) =
    seq {
        // We start with an element of score 0 to avoid errors
        yield 0<ScoreTier1>

        for _ in 1..roomsPerConfig do
            let roomLayout = generateRoomLayout random room config

            if validate room roomLayout |> Result.isOk then
                yield calculateScoreTier1 roomLayout
    }
    |> Seq.groupBy id
    |> Seq.maxBy fst
    |> snd
    |> Seq.sum

let rec simulateGenerations (random: Random) iterations configSeq =
    let configsToKeep =
        configSeq
        |> Seq.take (int (float configsPerGeneration * keepPercentage))
        |> Seq.cache

    let mutatedConfigs =
        configsToKeep
        // We mutate every config two times
        |> Seq.collect (fun config -> [ config; config ])
        |> Seq.map (mutateGenerationConfig random)

    let newRandomConfigs =
        generateGenerationConfigs random (int (float configsPerGeneration * mutatePercentage))

    let nextGeneration =
        configsToKeep
        |> Seq.append (generateGenerationConfigs random (int (float configsPerGeneration * randomPercentage)))
        |> Seq.sortByDescending (calculateGeneratorConfigScore random)

    if iterations <= 1 then
        nextGeneration
    else
        simulateGenerations random (iterations - 1) nextGeneration

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

    let startTime = DateTime.Now
    let seed = Random().Next()
    let random = Random seed

    // At first we use an evolutionary approach to find a fitting generation config
    let generationConfig =
        generateGenerationConfigs random configsPerGeneration
        |> Seq.sortByDescending (calculateGeneratorConfigScore random)
        |> simulateGenerations random configGenerations
        |> Seq.head

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

    let endTime = System.DateTime.Now

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
