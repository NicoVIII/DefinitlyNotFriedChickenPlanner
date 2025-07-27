module DefinitlyNotFriedChickenPlanner.RoomLayout.Scoring

open Microsoft.FSharp.Core.LanguagePrimitives

open DefinitlyNotFriedChickenPlanner

let countGrowboxes (roomLayout: RoomLayout) : int =
    roomLayout
    |> Set.filter (function
        | { applianceType = Growbox _ } -> true
        | _ -> false)
    |> Set.count

let inline floatWUnit (value: int<'a>) : float<'a> = float value |> FloatWithMeasure<'a>

let calculateHourlyCost (roomLayout: RoomLayout) =
    roomLayout
    |> Set.toList
    |> List.choose (function
        | { applianceType = Emitter emitter } -> Some emitter
        | _ -> None)
    |> List.sumBy (fun emitter ->
        match emitter.emitterType with
        | Heater value -> Config.heat.cost * floatWUnit value
        | Humidifier value -> Config.humidity.cost * floatWUnit value
        | Light value -> Config.light.cost * floatWUnit value
        | Sprinkler value -> Config.water.cost * floatWUnit value)

let calculateScoreTier1 roomLayout =
    countGrowboxes roomLayout * 1<ScoreTier1>

let calculateScoreTier2 roomLayout =
    calculateHourlyCost roomLayout * -1.0<ScoreTier2 * Hour / Dollar>
