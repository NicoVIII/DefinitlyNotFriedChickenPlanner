[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.GenerationConfig.Generation

open System

open DefinitlyNotFriedChickenPlanner

let generateGenerationConfig (random: Random) : GenerationConfig = {
    chanceForGrowbox = random.Next 21 * 5
    chanceForEmitter = random.Next 21 * 5
    chanceForOverhead = random.Next 21 * 5
}
