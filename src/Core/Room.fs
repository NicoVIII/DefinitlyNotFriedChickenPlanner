namespace DefinitlyNotFriedChickenPlanner

type Measurements = {
    heat: int<Heat>
    humidity: int<Humidity>
    light: int<Light>
    water: int<Water>
}

type Coordinate = { x: int; y: int }

type Room = int * int
