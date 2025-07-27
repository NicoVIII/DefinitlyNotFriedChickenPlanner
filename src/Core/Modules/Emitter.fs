[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DefinitlyNotFriedChickenPlanner.Emitter

let getValue emitter =
    match emitter.emitterType with
    | Heater value -> value * 1< / Heat>
    | Humidifier value -> value * 1< / Humidity>
    | Light value -> value * 1< / Light>
    | Sprinkler value -> value * 1< / Water>
