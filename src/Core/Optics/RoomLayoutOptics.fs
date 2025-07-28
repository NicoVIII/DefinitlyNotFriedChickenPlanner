namespace DefinitlyNotFriedChickenPlanner

open SimpleOptics

[<RequireQualifiedAccess>]
module EmitterOptic =
    let heater =
        Prism(
            (function
            | Heater h -> Some h
            | _ -> None),
            (fun _ v -> Heater v)
        )

    let humidifier =
        Prism(
            (function
            | Humidifier h -> Some h
            | _ -> None),
            (fun _ v -> Humidifier v)
        )

    let light =
        Prism(
            (function
            | Lamp l -> Some l
            | _ -> None),
            (fun _ v -> Lamp v)
        )

    let sprinkler =
        Prism(
            (function
            | Sprinkler w -> Some w
            | _ -> None),
            (fun _ v -> Sprinkler v)
        )

    let value =
        Lens(
            (function
            | Heater h -> h / 1y<Heat>
            | Humidifier h -> h / 1y<Humidity>
            | Lamp l -> l / 1y<Light>
            | Sprinkler w -> w / 1y<Water>),
            (fun e v ->
                match e with
                | Heater _ -> v * 1y<Heat> |> Heater
                | Humidifier _ -> v * 1y<Humidity> |> Humidifier
                | Lamp _ -> v * 1y<Light> |> Lamp
                | Sprinkler _ -> v * 1y<Water> |> Sprinkler)
        )

[<RequireQualifiedAccess>]
module ApplianceTypeOptic =
    let emitter =
        Prism(
            (function
            | Emitter e -> Some e
            | _ -> None),
            (fun _ v -> Emitter v)
        )

    let growbox =
        Prism(
            (function
            | Growbox g -> Some g
            | _ -> None),
            (fun _ v -> Growbox v)
        )

    let emitterValue = Optic.compose emitter EmitterOptic.value

[<RequireQualifiedAccess>]
module ApplianceOptic =
    let coordinate = Lens(_.coordinate, (fun a c -> { a with coordinate = c }))
    let applianceType = Lens(_.applianceType, (fun a t -> { a with applianceType = t }))

    // Composed optics
    let x = Optic.compose coordinate CoordinateOptic.x
    let y = Optic.compose coordinate CoordinateOptic.y
    let overhead = Optic.compose coordinate CoordinateOptic.overhead
    let emitter = Optic.compose applianceType ApplianceTypeOptic.emitter
    let heater = Optic.compose emitter EmitterOptic.heater
    let humidifier = Optic.compose emitter EmitterOptic.humidifier
    let light = Optic.compose emitter EmitterOptic.light
    let sprinkler = Optic.compose emitter EmitterOptic.sprinkler
    let growbox = Optic.compose applianceType ApplianceTypeOptic.growbox
    let emitterValue = Optic.compose applianceType ApplianceTypeOptic.emitterValue
