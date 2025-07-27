open RunHelpers
open RunHelpers.Shortcuts
open RunHelpers.Templates

[<RequireQualifiedAccess>]
module Config =
    let benchmarkProject =
        "./benchmark/DefinitelyNotFriedChickenPlanner.Benchmark.fsproj"

    let project = "./src/DefinitelyNotFriedChickenPlanner.fsproj"

module Task =
    let restore () =
        job {
            DotNet.toolRestore ()
            DotNet.restore Config.project
        }

    let build () =
        job { DotNet.build Config.project Debug }

    let run () = job { DotNet.run Config.project }

[<EntryPoint>]
let main args =
    args
    |> List.ofArray
    |> function
        | [ "restore" ] -> Task.restore ()
        | [ "build" ] ->
            job {
                Task.restore ()
                Task.build ()
            }
        | [ "benchmark" ] -> job { dotnet [ "run"; "--project"; Config.benchmarkProject; "-c"; "Release" ] }
        | [] -> job { DotNet.run Config.project }
        | args -> job { dotnet [ "run"; "--project"; Config.project; "--"; yield! args ] }
    |> Job.execute
