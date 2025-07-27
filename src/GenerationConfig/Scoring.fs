module DefinitlyNotFriedChickenPlanner.GenerationConfig.Scoring

open System

open DefinitlyNotFriedChickenPlanner
open DefinitlyNotFriedChickenPlanner.RoomLayout.Generation
open DefinitlyNotFriedChickenPlanner.RoomLayout.Scoring
open DefinitlyNotFriedChickenPlanner.RoomLayout.Validation

type CalculationConfig = { room: Room; roomsPerConfig: int }

let calculateGeneratorConfigScore (random: Random) calcConfig config =
    Seq.initInfinite (fun _ -> generateRoomLayout random calcConfig.room config)
    |> Seq.take calcConfig.roomsPerConfig
    |> Seq.filter (validate calcConfig.room >> Result.isOk)
    |> Seq.map calculateScoreTier1
    |> Seq.append (Seq.singleton 0<ScoreTier1>)
    |> Seq.groupBy id
    |> Seq.maxBy fst
    |> snd
    |> Seq.sum
