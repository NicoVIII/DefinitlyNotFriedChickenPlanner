[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.GenerationConfig.Generation

open Microsoft.FSharp.Core.Operators.Checked
open System

open DefinitlyNotFriedChickenPlanner

let generateGenerationConfig (random: Random) : GenerationConfig = {
    chances = {
        forHeater = random.Next 21 * 5
        forHumidifier = random.Next 21 * 5
        forSprinkler = random.Next 21 * 5
        forGrowbox = random.Next 21 * 5
        forLight = random.Next 21 * 5
    }
    optimizationStrategy =
        match random.Next 2 with
        | 0 -> HighestFirst
        | _ -> LowestFirst
}

let generateGenerationConfigs (random: Random) amount =
    Seq.initInfinite (fun _ -> generateGenerationConfig random)
    |> Seq.distinct
    |> Seq.take amount
