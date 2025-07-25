[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DefinitlyNotFriedChickenPlanner.Room

let generateCoords room =
    let width, height = room

    seq {
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                yield { x = x; y = y }
    }
    |> Seq.cache
