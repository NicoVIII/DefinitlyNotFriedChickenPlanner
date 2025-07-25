module DefinitlyNotFriedChickenPlanner.Printing

let printMeasurements (measurements: Measurements[,]) =
    let printGrid (getValue: Measurements -> int<'a>) =
        for y in 0 .. (Array2D.length1 measurements - 1) do
            for x in 0 .. (Array2D.length2 measurements - 1) do
                let value = getValue measurements.[x, y]
                printf "%3d " value

            printfn ""

    printfn "Heat:"
    printGrid (fun m -> m.heat)
    printfn "Humidity:"
    printGrid (fun m -> m.humidity)
    printfn "Light:"
    printGrid (fun m -> m.light)
    printfn "Water:"
    printGrid (fun m -> m.water)

let printAppliances (room: Room) (appliances: Appliance list) =
    let width, _ = room
    let roomCords = Room.generateCoords room

    let printGrid (appliances: Appliance list) =
        for coord in roomCords do
            match appliances |> List.tryFind (fun a -> a.coordinate = coord) with
            | Some appliance ->
                match appliance.applianceType with
                | Emitter emitter ->
                    match emitter.emitterType with
                    | Heater value -> printf "%3i%s " value "H"
                    | Humidifier value -> printf "%3i%s " value "U"
                    | Light value -> printf "%3i%s " value "L"
                    | Sprinkler value -> printf "%3i%s " value "S"
                | Growbox growbox ->
                    match growbox.orientation with
                    | North -> printf "%4s " "↑"
                    | East -> printf "%4s " "→"
                    | South -> printf "%4s " "↓"
                    | West -> printf "%4s " "←"
            | None -> printf "%4s " "-"

            if coord.x = width - 1 then
                printfn ""

        printfn ""

    printfn "Overhead:"

    appliances
    |> List.filter (function
        | {
              applianceType = Emitter { overhead = true }
          } -> true
        | _ -> false)
    |> printGrid

    printfn "Ground:"

    appliances
    |> List.filter (function
        | {
              applianceType = Emitter { overhead = true }
          } -> false
        | _ -> true)
    |> printGrid
