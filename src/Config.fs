[<RequireQualifiedAccess>]
module DefinitlyNotFriedChickenPlanner.Config

let heat = {|
    cost = 0.03<Dollar / Heat / Hour>
    min = -100<Heat>
    max = 100<Heat>
    reduction = 10<Heat / Tile>
|}

let humidity = {|
    cost = 0.02<Dollar / Humidity / Hour>
    min = -100<Humidity>
    max = 100<Humidity>
    reduction = 5<Humidity / Tile>
|}

let light = {|
    cost = 0.01<Dollar / Light / Hour>
    min = 0<Light>
    max = 100<Light>
    reduction = 10<Light / Tile>
|}

let water = {|
    cost = 0.03<Dollar / Water / Hour>
    min = 0<Water>
    max = 100<Water>
    reduction = 20<Water / Tile>
|}

let growboxTypes = {|
    cannabis = {
        minMeasurements = {
            heat = heat.min
            humidity = humidity.min
            light = 50<Light>
            water = 20<Water>
        }
        maxMeasurements = {
            heat = heat.max
            humidity = humidity.max
            light = light.max
            water = water.max
        }
    }
|}
