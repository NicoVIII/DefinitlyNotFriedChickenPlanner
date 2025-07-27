namespace DefinitlyNotFriedChickenPlanner.Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open DefinitlyNotFriedChickenPlanner
open DefinitlyNotFriedChickenPlanner.RoomLayout

type OptimizationBenchmark() =
    let room = 5, 5

    let growboxCoordinates = [
        { x = 1; y = 0 }
        { x = 3; y = 0 }
        { x = 0; y = 1 }
        { x = 3; y = 1 }
        { x = 0; y = 2 }
        { x = 3; y = 2 }
        { x = 0; y = 3 }
        { x = 3; y = 3 }
        { x = 1; y = 4 }
    ]

    let simpleRoomLayout =
        Set.ofList [
            {
                applianceType =
                    Emitter {
                        overhead = true
                        emitterType = Light 85<Light>
                    }
                coordinate = { x = 2; y = 2 }
            }
            {
                applianceType =
                    Emitter {
                        overhead = true
                        emitterType = Light 90<Light>
                    }
                coordinate = { x = 1; y = 1 }
            }
            {
                applianceType =
                    Emitter {
                        overhead = false
                        emitterType = Sprinkler 80<Water>
                    }
                coordinate = { x = 2; y = 2 }
            }
            {
                applianceType =
                    Emitter {
                        overhead = false
                        emitterType = Heater 70<Heat>
                    }
                coordinate = { x = 0; y = 0 }
            }
            {
                applianceType =
                    Emitter {
                        overhead = false
                        emitterType = Humidifier 75<Humidity>
                    }
                coordinate = { x = 4; y = 4 }
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

    [<Benchmark>]
    member _.Optimize() =
        optimizeEmitterCost LowestFirst room simpleRoomLayout

module Program =
    [<EntryPoint>]
    let main _ =
        BenchmarkRunner.Run<OptimizationBenchmark>() |> ignore

        0 // Return an integer exit code
