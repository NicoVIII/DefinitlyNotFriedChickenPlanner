namespace DefinitlyNotFriedChickenPlanner

type Measure =
    | Heat
    | Humidity
    | Light
    | Water

type Measurements = {
    heat: int8<Heat>
    humidity: int8<Humidity>
    light: int8<Light>
    water: int8<Water>
}

type Coordinate = { overhead: bool; x: uint8; y: uint8 }
type Room = { height: uint8; width: uint8 }
