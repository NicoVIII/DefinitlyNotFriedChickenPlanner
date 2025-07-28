module DefinitlyNotFriedChickenPlanner.Tests.RoomLayout.Optimization

open Expecto

open DefinitlyNotFriedChickenPlanner
open DefinitlyNotFriedChickenPlanner.RoomLayout

let sampleRoom: Room = { width = 3uy; height = 3uy }

[<Tests>]
let tests =
    testList "RoomLayout.Optimization.optimizeEmitterCost" [
        testCase "removes all emitters, if there are no growboxes"
        <| fun _ ->
            // Arrange
            let layout = [
                buildHeater { overhead = false; x = 0uy; y = 0uy }
                buildHumidifier { overhead = false; x = 1uy; y = 0uy }
                buildSprinkler { overhead = false; x = 2uy; y = 0uy }
                buildLight { overhead = true; x = 1uy; y = 1uy }
            ]

            // Act
            let optimized = optimizeEmitterCost HighestFirst sampleRoom layout

            // Assert
            Expect.isEmpty optimized "Optimized layout should be empty when no growboxes are present"

        testCase "reduces emitters when growboxes are present"
        <| fun _ ->
            // Arrange
            let layout = [
                buildLight { overhead = true; x = 1uy; y = 1uy }
                buildGrowbox East { overhead = false; x = 1uy; y = 1uy }
                buildSprinkler { overhead = false; x = 1uy; y = 0uy }
            ]

            // Act
            let optimized = optimizeEmitterCost HighestFirst sampleRoom layout

            // Assert
            Expect.hasLength optimized 3 "Optimized layout should contain 3 appliances"

            Expect.exists
                optimized
                (fun a ->
                    match a.applianceType with
                    | Emitter(Light 50y<Light>) -> true
                    | _ -> false)
                "Optimized layout should contain a light with value 50"

            Expect.exists
                optimized
                (fun a ->
                    match a.applianceType with
                    | Emitter(Sprinkler 40y<Water>) -> true
                    | _ -> false)
                "Optimized layout should contain a sprinkler with value 20"

            Expect.exists optimized (fun a -> a.applianceType.IsGrowbox) "Optimized layout should contain a growbox"
    ]
