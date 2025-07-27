[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.GenerationConfig.Mutation

open Microsoft.FSharp.Core.Operators.Checked
open System

open DefinitlyNotFriedChickenPlanner

type MutationConfig = { mutationStep: int }

let determineFactor (random: Random) value =
    if value >= 100 then -1
    elif value <= 0 then 1
    elif random.Next 2 = 1 then 1
    else -1

let mutateChance
    (random: Random)
    mutConfig
    (fieldSelector: GenerationConfigChances -> int)
    (fieldUpdater: int -> GenerationConfigChances -> GenerationConfigChances)
    (config: GenerationConfig)
    =
    let value = fieldSelector config.chances
    let factor = determineFactor random value
    let mutation = factor * mutConfig.mutationStep

    {
        config with
            chances = config.chances |> fieldUpdater (value + mutation)
    }

let mutateGenerationConfig (random: Random) mutConfig (config: GenerationConfig) =
    match random.Next 6 with
    | 0 -> mutateChance random mutConfig _.forGrowbox (fun v c -> { c with forGrowbox = v }) config
    | 1 -> mutateChance random mutConfig _.forHeater (fun v c -> { c with forHeater = v }) config
    | 2 -> mutateChance random mutConfig _.forHumidifier (fun v c -> { c with forHumidifier = v }) config
    | 3 -> mutateChance random mutConfig _.forSprinkler (fun v c -> { c with forSprinkler = v }) config
    | 4 -> mutateChance random mutConfig _.forLight (fun v c -> { c with forLight = v }) config
    | _ -> {
        config with
            optimizationStrategy =
                match config.optimizationStrategy with
                | HighestFirst -> LowestFirst
                | LowestFirst -> HighestFirst
      }
