[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.RoomLayout.Printing

open DefinitlyNotFriedChickenPlanner

let printMeasurements (measurements: Measurements[,]) =
    let printGrid (getValue: Measurements -> int8<'a>) =
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

let printRoomLayout (room: Room) (roomLayout: RoomLayout) =
    let printGrid overhead (roomLayout: RoomLayout) =
        for coord in Room.generateCoords overhead room do
            match roomLayout |> Set.toList |> List.tryFind (fun a -> a.coordinate = coord) with
            | Some appliance ->
                match appliance.applianceType with
                | Emitter emitter ->
                    match emitter with
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

            if coord.x = room.width - 1uy then
                printfn ""

        printfn ""

    printfn "Overhead:"
    roomLayout |> Set.filter (_.coordinate >> _.overhead) |> printGrid true

    printfn "Ground:"
    roomLayout |> Set.filter (_.coordinate >> _.overhead >> not) |> printGrid false
