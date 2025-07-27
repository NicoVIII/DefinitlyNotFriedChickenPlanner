[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DefinitlyNotFriedChickenPlanner.Room

let generateBasicCoords room =
    seq {
        for y in 0uy .. room.height - 1uy do
            for x in 0uy .. room.width - 1uy do
                yield {| x = x; y = y |}
    }

let generateCoords overhead room =
    generateBasicCoords room
    |> Seq.map (fun coord -> {
        overhead = overhead
        x = coord.x
        y = coord.y
    })
