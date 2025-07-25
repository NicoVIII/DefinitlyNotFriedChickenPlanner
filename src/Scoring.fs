module DefinitlyNotFriedChickenPlanner.Scoring

open Microsoft.FSharp.Core.LanguagePrimitives

let countGrowboxes (appliances: Appliance list) : int =
    appliances
    |> List.filter (function
        | { applianceType = Growbox _ } -> true
        | _ -> false)
    |> List.length

let inline floatWUnit (value: int<'a>) : float<'a> = float value |> FloatWithMeasure<'a>

let calculateHourlyCost (appliances: Appliance list) : float =
    appliances
    |> List.choose (function
        | { applianceType = Emitter emitter } -> Some emitter
        | _ -> None)
    |> List.sumBy (fun emitter ->
        match emitter.emitterType with
        | Heater value -> Config.heat.cost * floatWUnit value
        | Humidifier value -> Config.humidity.cost * floatWUnit value
        | Light value -> Config.light.cost * floatWUnit value
        | Sprinkler value -> Config.water.cost * floatWUnit value)
    |> (*) 1.0<Hour / Dollar>

let calculateApplianceScore (appliances: Appliance list) =
    let growboxCount = countGrowboxes appliances
    let cost = calculateHourlyCost appliances * -1.0

    // Score is simply the number of growboxes for now
    // This can be extended to include more complex scoring logic
    growboxCount, cost
