open RunHelpers
open RunHelpers.Shortcuts
open RunHelpers.Templates

[<RequireQualifiedAccess>]
module Config =
    let benchmarkProject =
        "./benchmark/DefinitelyNotFriedChickenPlanner.Benchmark.fsproj"

    let project = "./src/DefinitelyNotFriedChickenPlanner.fsproj"
    let testProject = "./tests/DefinitelyNotFriedChickenPlanner.Tests.fsproj"

    let projects = [ project; testProject; benchmarkProject ]

module Task =
    let restore () =
        job {
            DotNet.toolRestore ()

            for project in Config.projects do
                DotNet.restore project
        }

    let build () =
        job {
            for project in Config.projects do
                DotNet.build project Debug
        }

    let benchmark () =
        job { dotnet [ "run"; "--project"; Config.benchmarkProject; "-c"; "Release" ] }

    let run () = job { DotNet.run Config.project }

    let runWithArgs args =
        job { dotnet [ "run"; "--project"; Config.project; "--"; yield! args ] }

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
        | [ "benchmark" ] -> Task.benchmark ()
        | [ "test" ]
        | [ "tests" ] -> job { DotNet.run Config.testProject }
        | [] -> Task.run ()
        | args -> Task.runWithArgs args
    |> Job.execute
