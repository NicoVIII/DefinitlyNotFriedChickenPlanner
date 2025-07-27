module DefinitlyNotFriedChickenPlanner.GenerationConfig.Evolution

open System

open DefinitlyNotFriedChickenPlanner
open DefinitlyNotFriedChickenPlanner.GenerationConfig.Generation
open DefinitlyNotFriedChickenPlanner.GenerationConfig.Mutation
open DefinitlyNotFriedChickenPlanner.GenerationConfig.Scoring

type SimulationConfig = {
    calculationConfig: CalculationConfig
    configsPerGeneration: int
    keepPercentage: float
    mutatePercentage: float
    mutationConfig: MutationConfig
}

let printGeneration generation =
    generation
    |> Seq.truncate 3
    |> Seq.iteri (fun i config -> printfn "%2i - %A" i config)

let simulateGenerations (random: Random) simConfig generationGoal =
    let sortByScoreDescending =
        Seq.sortByDescending (calculateGeneratorConfigScore random simConfig.calculationConfig)

    let calculatePercentageOfGeneration percentage =
        float simConfig.configsPerGeneration * percentage |> round |> int

    let amountToKeep = calculatePercentageOfGeneration simConfig.keepPercentage
    let amountToMutate = calculatePercentageOfGeneration simConfig.mutatePercentage

    let amountToGenerate =
        simConfig.configsPerGeneration - amountToKeep - amountToMutate

    let rec simulateGeneration i configSeq =
        let configsToKeep = configSeq |> Seq.take amountToKeep |> Seq.cache

        let mutatedConfigs =
            configsToKeep
            |> Seq.cycle
            |> Seq.take amountToMutate
            |> Seq.map (mutateGenerationConfig random simConfig.mutationConfig)

        let generatedConfigs = generateGenerationConfigs random amountToGenerate

        let nextGeneration =
            [ configsToKeep; mutatedConfigs; generatedConfigs ]
            |> Seq.collect id
            |> Seq.sortByDescending (calculateGeneratorConfigScore random simConfig.calculationConfig)

#if DEBUG
        printfn "Generation %d:" i
        printGeneration nextGeneration
#endif

        if i >= generationGoal then
            nextGeneration
        else
            simulateGeneration (i + 1) nextGeneration

    // Generate the starting generation
    generateGenerationConfigs random simConfig.configsPerGeneration
    |> sortByScoreDescending
#if DEBUG
    |> (fun generation ->
        printfn "Initial Generation:"
        printGeneration generation
        generation)
#endif
    // Simulate the generations until the goal is reached
    |> simulateGeneration 1
