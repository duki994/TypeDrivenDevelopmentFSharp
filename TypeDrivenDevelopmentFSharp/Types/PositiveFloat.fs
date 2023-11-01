namespace TypeDrivenDevelopmentFSharp.Types

open System

type PositiveFloat = private PositiveFloat of float

type PositiveFloatValidationErrors =
    | NegativeValue of float
    | ZeroValue

module PositiveFloat = 
    let create value =
        if value < 0.0 then Error(NegativeValue value)
        else if value = 0.0 then Error ZeroValue
        else Ok(PositiveFloat value)

    let value (PositiveFloat value) = value

    let multiply values =
        values
        |> Seq.map value
        |> Seq.reduce (fun acc value -> acc * value)
        |> PositiveFloat

    module Constants =
        let PI = PositiveFloat Math.PI
        let Tau = PositiveFloat Math.Tau
        let E = PositiveFloat Math.E