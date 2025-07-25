module DefinitlyNotFriedChickenPlanner.Program

open Generation
open Validation
open Scoring
open Printing
open Optimisation

// Run code
[<EntryPoint>]
let main args =
    let room = 5, 5

    // Read the first arg as iteration count if provided, otherwise use default
    let defaultIterations = 1000

    let iterations =
        if args.Length > 0 then
            match System.Int32.TryParse(args.[0]) with
            | true, value -> value
            | _ -> defaultIterations
        else
            defaultIterations

    // Use the second arg as seed if provided, otherwise generate a random seed
    let seed =
        if args.Length > 1 then
            match System.Int32.TryParse(args.[1]) with
            | true, value -> value
            | _ -> System.Random().Next()
        else
            System.Random().Next()

    printfn "Generating appliances using seed %i for room size %A..." seed room
    let startTime = System.DateTime.Now
    let random = System.Random seed

    let mutable bestAppliancesList = []
    let mutable score = 0, 0.0
    let mutable validRooms = 0
    let mutable validationErrors = Map.empty<ValidationError, uint>

    for _ in 1..iterations do
        let appliances = generateAppliances random room

        match validate room appliances with
        | Ok() ->
            validRooms <- validRooms + 1

            let newScore = calculateApplianceScore appliances

            match bestAppliancesList, score with
            | [], _ ->
                // If this is the first valid appliance configuration, we optimize it and set it as best
                let optimizedAppliances = optimiseEmitterCost random room appliances
                let optimizedScore = calculateApplianceScore optimizedAppliances
                bestAppliancesList <- [ optimizedAppliances ]
                score <- optimizedScore
            | _, (currentGrowboxCount, currentCost) ->
                let newGrowboxCount, _ = newScore

                if newGrowboxCount < currentGrowboxCount then
                    // This room is clearly worse, skip it
                    ()
                else
                    // This room has a chance to be better, therefore we optimize it
                    let optimizedAppliances = optimiseEmitterCost random room appliances
                    let optimizedScore = calculateApplianceScore optimizedAppliances
                    let newGrowboxCount, optimizedCost = optimizedScore

                    if
                        newGrowboxCount > currentGrowboxCount
                        || newGrowboxCount = currentGrowboxCount && optimizedCost > currentCost
                    then
                        bestAppliancesList <- [ optimizedAppliances ]
                        score <- optimizedScore
                    elif optimizedScore = score then
                        // If the score is equal, add to the list of best appliances
                        bestAppliancesList <- optimizedAppliances :: bestAppliancesList

        | Error error ->
            // Increase amount in map by one
            validationErrors <- Map.change error (Option.defaultValue 0u >> (+) 1u >> Some) validationErrors

    let endTime = System.DateTime.Now

    match bestAppliancesList with
    | [] -> printfn "No valid appliance configuration found."
    | _ ->
        List.iter (printAppliances room) bestAppliancesList
        let growboxCount, cost = score

        printfn
            "Found %i valid configurations with %i growboxes and cost %.2f per hour"
            (List.length bestAppliancesList)
            growboxCount
            (cost * -1.)

    printfn "Total time taken: %A" (endTime - startTime)
    printfn "Room generation completed with %i (%i%%) valid rooms." validRooms (validRooms * 100 / iterations)

#if DEBUG
    printfn "[DEBUG] Validation errors:"

    validationErrors
    |> Map.toSeq
    |> Seq.sortByDescending snd
    |> Seq.iter (fun (error, count) -> printfn "%4i - %A" count error)
#endif

    0 // Return an integer exit code
