namespace DefinitlyNotFriedChickenPlanner.Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open SimpleOptics

open DefinitlyNotFriedChickenPlanner
open DefinitlyNotFriedChickenPlanner.RoomLayout

type OptimizationBenchmark() =
    let room = { width = 15uy; height = 10uy }

    let growboxCoordinates =
        [ 1, 0; 3, 0; 0, 1; 3, 1; 0, 2; 3, 2; 0, 3; 3, 3; 1, 4 ]
        |> List.map (fun (x, y) -> {
            overhead = false
            x = uint8 x
            y = uint8 y
        })

    let lightCoordinates =
        [ 2, 2; 1, 1 ]
        |> List.map (fun (x, y) -> {
            overhead = true
            x = uint8 x
            y = uint8 y
        })

    let simpleTileLayout = [
        {
            applianceType = Emitter(Sprinkler 80y<Water>)
            coordinate = { overhead = false; x = 2uy; y = 2uy }
        }
        {
            applianceType = Emitter(Heater 70y<Heat>)
            coordinate = { overhead = false; x = 0uy; y = 0uy }
        }
        {
            applianceType = Emitter(Humidifier 75y<Humidity>)
            coordinate = { overhead = false; x = 4uy; y = 4uy }
        }
        for coord in lightCoordinates do
            {
                applianceType = Emitter(Light 90y<Light>)
                coordinate = coord
            }
        for coord in growboxCoordinates do
            {
                applianceType =
                    Growbox {
                        orientation = East
                        growboxType = Config.growboxTypes.cannabis
                    }
                coordinate = coord
            }
    ]

    let simpleRoomLayout = [
        for appliance in simpleTileLayout do
            for y in 0uy .. 5uy .. 5uy do
                for x in 0uy .. 5uy .. 10uy do
                    yield
                        appliance
                        |> Optic.map ApplianceOptic.x ((+) x)
                        |> Optic.map ApplianceOptic.y ((+) y)
    ]

    [<Benchmark>]
    member _.CalculateScoreTier1() = calculateScoreTier1 simpleRoomLayout

    [<Benchmark>]
    member _.CalculateScoreTier2() = calculateScoreTier2 simpleRoomLayout

    [<Benchmark>]
    member _.Validate() =
        validate room simpleRoomLayout |> ignore

    [<Benchmark>]
    member _.OptimizeLowestFirst() =
        optimizeEmitterCost LowestFirst room simpleRoomLayout

    [<Benchmark>]
    member _.OptimizeHeighestFirst() =
        optimizeEmitterCost HighestFirst room simpleRoomLayout

module Program =
    [<EntryPoint>]
    let main _ =
        BenchmarkRunner.Run<OptimizationBenchmark>() |> ignore

        0 // Return an integer exit code
