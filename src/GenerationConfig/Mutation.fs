module DefinitlyNotFriedChickenPlanner.GenerationConfig.Mutation

open System

open DefinitlyNotFriedChickenPlanner

type MutationConfig = { mutationStep: int }

let determineFactor (random: Random) value =
    if value >= 100 then -1
    elif value <= 0 then 1
    elif random.Next 2 = 1 then 1
    else -1

let mutateGenerationConfig (random: Random) mutConfig (config: GenerationConfig) =
    let factor = if random.Next 2 = 1 then 1 else -1
    let mutation = factor * mutConfig.mutationStep

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
