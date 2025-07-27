[<RequireQualifiedAccess>]
module DefinitlyNotFriedChickenPlanner.Config

open DefinitlyNotFriedChickenPlanner.Helper

let heat = {|
    cost = 0.03<Dollar / Heat / Hour>
    min = -100y<Heat>
    max = 100s<Heat>
    reduction = 10s<Heat / Tile>
|}

let humidity = {|
    cost = 0.02<Dollar / Humidity / Hour>
    min = -100y<Humidity>
    max = 100s<Humidity>
    reduction = 5s<Humidity / Tile>
|}

let light = {|
    cost = 0.01<Dollar / Light / Hour>
    min = 0y<Light>
    max = 100s<Light>
    reduction = 10s<Light / Tile>
|}

let water = {|
    cost = 0.03<Dollar / Water / Hour>
    min = 0y<Water>
    max = 100s<Water>
    reduction = 20s<Water / Tile>
|}

let growboxTypes = {|
    cannabis = {
        minMeasurements = {
            heat = heat.min
            humidity = humidity.min
            light = 50y<Light>
            water = 20y<Water>
        }
        maxMeasurements = {
            heat = int16ToInt8 heat.max
            humidity = int16ToInt8 humidity.max
            light = int16ToInt8 light.max
            water = int16ToInt8 water.max
        }
    }
|}
