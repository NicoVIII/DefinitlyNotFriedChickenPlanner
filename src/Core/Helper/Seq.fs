[<RequireQualifiedAccess>]
module DefinitlyNotFriedChickenPlanner.Seq

let cycle (source: seq<'T>) : seq<'T> =
    seq {
        let cache = source |> Seq.toArray

        if cache.Length = 0 then
            yield! Seq.empty
        else
            while true do
                yield! cache
    }
