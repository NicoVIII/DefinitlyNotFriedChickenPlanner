[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.GenerationConfigTypes

type GenerationConfig = {
    chanceForGrowbox: int
    chanceForEmitter: int
    chanceForOverhead: int
// TODO: min/max values for emitters
// TODO: optimization strategy (highest first, lowest first, outer first(?))
}
