[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.GenerationConfigTypes

type OptimizationStrategy =
    | HighestFirst
    | LowestFirst
// TODO: OuterFirst

type GenerationConfigChances = {
    forGrowbox: int
    forHeater: int
    forHumidifier: int
    forLight: int
    forSprinkler: int
}

type GenerationConfig = {
    chances: GenerationConfigChances
    optimizationStrategy: OptimizationStrategy
// TODO: min/max values for emitters
}
