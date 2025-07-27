[<AutoOpen>]
module DefinitlyNotFriedChickenPlanner.Helper.Measure

open Microsoft.FSharp.Core.LanguagePrimitives

let inline int8ToDouble (value: int8<'a>) : double<'a> = float value |> FloatWithMeasure<'a>
let inline int8ToInt16 (value: int8<'a>) : int16<'a> = int16 value |> Int16WithMeasure<'a>
let inline int16ToInt8 (value: int16<'a>) : int8<'a> = int8 value |> SByteWithMeasure<'a>

let inline uint8Toint16 (value: uint8<'a>) : int16<'a> = int16 value |> Int16WithMeasure<'a>
