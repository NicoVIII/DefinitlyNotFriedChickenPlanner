[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.GenerationConfig.Scoring

open Microsoft.FSharp.Core.Operators.Checked
open System

open DefinitlyNotFriedChickenPlanner
open DefinitlyNotFriedChickenPlanner.RoomLayout

type CalculationConfig = { room: Room; roomsPerConfig: int }

let calculateGeneratorConfigScore (random: Random) calcConfig config =
    Seq.initInfinite (fun _ -> generateRoomLayout random calcConfig.room config)
    |> Seq.take calcConfig.roomsPerConfig
    // We only count valid room layouts
    |> Seq.filter (validate calcConfig.room >> Result.isOk)
    |> Seq.map (optimizeEmitterCost config.optimizationStrategy calcConfig.room)
    |> Seq.map calculateScore
    |> Seq.append (Seq.singleton nullScore)
    |> Seq.groupBy id
    |> Seq.maxBy fst
    |> snd
    |> Seq.fold (fun sum (scoreTier1, scoreTier2) -> fst sum + scoreTier1, snd sum + scoreTier2) nullScore
