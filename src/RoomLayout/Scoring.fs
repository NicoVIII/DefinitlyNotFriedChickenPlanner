[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.RoomLayout.Scoring

open DefinitlyNotFriedChickenPlanner
open DefinitlyNotFriedChickenPlanner.Helper
open Microsoft.FSharp.Core.Operators.Checked

let countGrowboxes (roomLayout: RoomLayout) : int =
    roomLayout |> List.filter (fun a -> a.applianceType.IsGrowbox) |> List.length

let calculateHourlyCost (roomLayout: RoomLayout) =
    roomLayout
    |> List.choose (function
        | { applianceType = Emitter emitter } -> Some emitter
        | _ -> None)
    |> List.sumBy (fun emitter ->
        match emitter with
        | Heater value -> Config.heat.cost * int8ToDouble value
        | Humidifier value -> Config.humidity.cost * int8ToDouble value
        | Light value -> Config.light.cost * int8ToDouble value
        | Sprinkler value -> Config.water.cost * int8ToDouble value)

let calculateScoreTier1 roomLayout =
    countGrowboxes roomLayout * 1<ScoreTier1>

let calculateScoreTier2 roomLayout =
    calculateHourlyCost roomLayout * -1.0<ScoreTier2 * Hour / Dollar>

let nullScore = 0<ScoreTier1>, 0.<ScoreTier2>

let calculateScore roomLayout =
    let scoreTier1 = calculateScoreTier1 roomLayout
    let scoreTier2 = calculateScoreTier2 roomLayout
    scoreTier1, scoreTier2
