module DefinitlyNotFriedChickenPlanner.Tests.RoomLayout.Validation

open Expecto

open DefinitlyNotFriedChickenPlanner
open DefinitlyNotFriedChickenPlanner.RoomLayout.Generation
open DefinitlyNotFriedChickenPlanner.RoomLayout.Validation

let smallRoom: Room = { width = 3uy; height = 3uy }
let biggerRoom: Room = { width = 7uy; height = 7uy }

[<Tests>]
let tests =
    testList "RoomLayout.Validation" [
        testList "calculateMeasurements" [
            testCase "returns correct measurements for light"
            <| fun _ ->
                // Arrange
                let layout = [ buildLight { overhead = true; x = 0uy; y = 0uy } ]

                // Act
                let measurements = calculateMeasurements smallRoom layout

                // Assert
                let expectedRows =
                    [ [ 90y; 80y; 70y ]; [ 80y; 70y; 60y ]; [ 70y; 60y; 50y ] ]
                    |> List.map (List.map (fun x -> x * 1y<Light>))

                for y in 0..2 do
                    for x in 0..2 do
                        Expect.equal
                            measurements.[x, y].light
                            expectedRows.[y].[x]
                            $"Expected different result at ({x}, {y})"

            testCase "returns correct measurements for heater"
            <| fun _ ->
                // Arrange
                let layout = [ buildHeater { overhead = false; x = 0uy; y = 0uy } ]

                // Act
                let measurements = calculateMeasurements smallRoom layout

                // Assert
                let expectedRows =
                    [ [ 70y; 60y; 50y ]; [ 60y; 50y; 40y ]; [ 50y; 40y; 30y ] ]
                    |> List.map (List.map (fun x -> x * 1y<Heat>))

                for y in 0..2 do
                    for x in 0..2 do
                        Expect.equal
                            measurements.[x, y].heat
                            expectedRows.[y].[x]
                            $"Expected different result at ({x}, {y})"

            testCase "returns correct measurements for humidifier"
            <| fun _ ->
                // Arrange
                let layout = [ buildHumidifier { overhead = false; x = 0uy; y = 0uy } ]

                // Act
                let measurements = calculateMeasurements smallRoom layout

                // Assert
                let expectedRows =
                    [ [ 75y; 70y; 65y ]; [ 70y; 65y; 60y ]; [ 65y; 60y; 55y ] ]
                    |> List.map (List.map (fun x -> x * 1y<Humidity>))

                for y in 0..2 do
                    for x in 0..2 do
                        Expect.equal
                            measurements.[x, y].humidity
                            expectedRows.[y].[x]
                            $"Expected different result at ({x}, {y})"

            testCase "returns correct measurements for sprinkler"
            <| fun _ ->
                // Arrange
                let layout = [ buildSprinkler { overhead = false; x = 0uy; y = 0uy } ]

                // Act
                let measurements = calculateMeasurements smallRoom layout

                // Assert
                let expectedRows =
                    [ [ 80y; 60y; 40y ]; [ 60y; 40y; 20y ]; [ 40y; 20y; 0y ] ]
                    |> List.map (List.map (fun x -> x * 1y<Water>))

                for y in 0..2 do
                    for x in 0..2 do
                        Expect.equal
                            measurements.[x, y].water
                            expectedRows.[y].[x]
                            $"Expected different result at ({x}, {y})"

            testCase "returns correct measurements for single light"
            <| fun _ ->
                // Arrange
                let layout = [ buildLight { overhead = true; x = 1uy; y = 2uy } ]

                // Act
                let measurements = calculateMeasurements biggerRoom layout

                // Assert
                let expectedRows =
                    [
                        [ 60y; 70y; 60y; 50y; 40y; 30y; 20y ]
                        [ 70y; 80y; 70y; 60y; 50y; 40y; 30y ]
                        [ 80y; 90y; 80y; 70y; 60y; 50y; 40y ]
                        [ 70y; 80y; 70y; 60y; 50y; 40y; 30y ]
                        [ 60y; 70y; 60y; 50y; 40y; 30y; 20y ]
                        [ 50y; 60y; 50y; 40y; 30y; 20y; 10y ]
                        [ 40y; 50y; 40y; 30y; 20y; 10y; 0y ]
                    ]
                    |> List.map (List.map (fun x -> x * 1y<Light>))

                for y in 0..6 do
                    for x in 0..6 do
                        Expect.equal
                            measurements.[x, y].light
                            expectedRows.[y].[x]
                            $"Expected different result at ({x}, {y})"

            testCase "returns correct measurements for multiple lights"
            <| fun _ ->
                // Arrange
                let layout = [
                    buildLight { overhead = true; x = 1uy; y = 2uy }
                    buildLight { overhead = true; x = 6uy; y = 5uy }
                ]

                // Act
                let measurements = calculateMeasurements biggerRoom layout

                // Assert
                let expectedRows =
                    [
                        [ 60y; 70y; 60y; 60y; 60y; 60y; 60y ]
                        [ 70y; 80y; 80y; 80y; 80y; 80y; 80y ]
                        [ 80y; 100y; 100y; 100y; 100y; 100y; 100y ]
                        [ 80y; 100y; 100y; 100y; 100y; 100y; 100y ]
                        [ 80y; 100y; 100y; 100y; 100y; 100y; 100y ]
                        [ 80y; 100y; 100y; 100y; 100y; 100y; 100y ]
                        [ 60y; 80y; 80y; 80y; 80y; 80y; 80y ]
                    ]
                    |> List.map (List.map (fun x -> x * 1y<Light>))

                for y in 0..6 do
                    for x in 0..6 do
                        Expect.equal
                            measurements.[x, y].light
                            expectedRows.[y].[x]
                            $"Expected different result at ({x}, {y})"

            testCase "returns correct measurements for very high values"
            <| fun _ ->
                // Arrange
                let layout = [
                    buildLight { overhead = true; x = 0uy; y = 0uy }
                    buildLight { overhead = true; x = 2uy; y = 0uy }
                    buildLight { overhead = true; x = 1uy; y = 1uy }
                    buildLight { overhead = true; x = 0uy; y = 2uy }
                    buildLight { overhead = true; x = 2uy; y = 2uy }
                ]

                // Act
                let measurements = calculateMeasurements smallRoom layout

                // Assert
                for y in 0..2 do
                    for x in 0..2 do
                        Expect.equal measurements.[x, y].light 100y<Light> $"Expected different result at ({x}, {y})"
        ]
        testList "validate" [
            testCase "returns Ok for valid layout"
            <| fun _ ->
                // Arrange
                let validLayout =
                    Set.ofList [
                        buildHeater { overhead = false; x = 0uy; y = 0uy }
                        buildHumidifier { overhead = false; x = 1uy; y = 0uy }
                        buildSprinkler { overhead = false; x = 2uy; y = 0uy }
                        buildLight { overhead = true; x = 1uy; y = 1uy }
                        buildGrowbox East { overhead = false; x = 1uy; y = 1uy }
                    ]

                // Act
                let result = validate smallRoom validLayout

                // Assert
                Expect.isOk result "Expected Ok for valid layout"

            testCase "returns Error for double placement"
            <| fun _ ->
                // Arrange
                let doublePlacementLayout =
                    Set.ofList [
                        buildHeater { overhead = false; x = 0uy; y = 0uy }
                        buildSprinkler { overhead = false; x = 0uy; y = 0uy }
                    ]

                // Act
                let result = validate smallRoom doublePlacementLayout

                // Assert
                Expect.isError result "Expected Error for invalid layout"
                Expect.equal result (DoublePlacement |> Placement |> Error) "Expected DoublePlacement error"

            testCase "returns Error for out of bounds coordinate"
            <| fun _ ->
                // Arrange
                let outOfBoundsLayout =
                    Set.ofList [
                        buildHeater { overhead = false; x = 3uy; y = 0uy } // Out of bounds
                    ]

                // Act
                let result = validate smallRoom outOfBoundsLayout

                // Assert
                Expect.isError result "Expected Error for out of bounds coordinate"
                Expect.equal result (InvalidCoordinate |> Placement |> Error) "Expected InvalidCoordinate error"

            testCase "returns Error for invalid overhead setting"
            <| fun _ ->
                // Arrange
                let invalidOverheadLayout =
                    Set.ofList [ buildHeater { overhead = true; x = 0uy; y = 0uy } ]

                // Act
                let result = validate smallRoom invalidOverheadLayout

                // Assert
                Expect.isError result "Expected Error for invalid overhead setting"
                Expect.equal result (InvalidOverheadSetting |> Placement |> Error) "Expected InvalidCoordinate error"

            testCase "returns Error for invalid growbox placement - out of bounds"
            <| fun _ ->
                // Arrange
                let invalidGrowboxLayout =
                    Set.ofList [ buildGrowbox East { overhead = false; x = 2uy; y = 0uy } ]

                // Act
                let result = validate smallRoom invalidGrowboxLayout

                // Assert
                Expect.isError result "Expected Error for invalid growbox placement"
                Expect.equal result (GrowboxOutOfBounds |> Placement |> Error) "Expected InvalidCoordinate error"

            testCase "returns Error for invalid growbox placement - no free space"
            <| fun _ ->
                // Arrange
                let invalidGrowboxLayout =
                    Set.ofList [
                        buildGrowbox East { overhead = false; x = 1uy; y = 1uy }
                        buildHeater { overhead = false; x = 2uy; y = 1uy } // Overlaps with growbox
                    ]

                // Act
                let result = validate smallRoom invalidGrowboxLayout

                // Assert
                Expect.isError result "Expected Error for invalid growbox placement"

                Expect.equal
                    result
                    (GrowboxAdjacentTileOccupied |> Placement |> Error)
                    "Expected InvalidGrowboxPlacement error"

            testCase "returns Error for invalid growbox placement - free space shared"
            <| fun _ ->
                // Arrange
                let invalidGrowboxLayout =
                    Set.ofList [
                        buildGrowbox East { overhead = false; x = 0uy; y = 1uy }
                        buildGrowbox West { overhead = false; x = 2uy; y = 1uy }
                    ]

                // Act
                let result = validate smallRoom invalidGrowboxLayout

                // Assert
                Expect.isError result "Expected Error for invalid growbox placement"

                Expect.equal
                    result
                    (GrowboxAdjacentTileNeeded |> Placement |> Error)
                    "Expected InvalidGrowboxPlacement error"
        ]
    ]
