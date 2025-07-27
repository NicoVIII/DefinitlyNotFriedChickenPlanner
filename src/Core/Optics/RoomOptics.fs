namespace DefinitlyNotFriedChickenPlanner

open SimpleOptics

[<RequireQualifiedAccess>]
module CoordinateOptic =
    let x = Lens(_.x, (fun c x -> { c with x = x }))
    let y = Lens(_.y, (fun c y -> { c with y = y }))
    let overhead = Lens(_.overhead, (fun c o -> { c with overhead = o }))
