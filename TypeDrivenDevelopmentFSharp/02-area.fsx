// Types
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


type RectangleType =
    { Width: PositiveFloat
      Height: PositiveFloat }

type CircleType = { Radius: PositiveFloat }

type EllipseType =
    { SemiMajorAxis: PositiveFloat
      SemiMinorAxis: PositiveFloat }

type Shape =
    | Rectangle of RectangleType
    | Circle of CircleType
    | Ellipse of EllipseType

type Area = Area of PositiveFloat
type AreaCalculator = Shape -> Area

module AreaCalculator =
    module private Calculators =
        let rectangle (r: RectangleType) =
            Area(PositiveFloat.multiply [ r.Width; r.Height ])

        let circle (c: CircleType) =
            Area(PositiveFloat.multiply [ PositiveFloat.Constants.PI; c.Radius; c.Radius ])

        let ellipse (e: EllipseType) =
            Area(PositiveFloat.multiply [ PositiveFloat.Constants.PI; e.SemiMajorAxis; e.SemiMinorAxis ])

    let calculate: AreaCalculator =
        fun shape ->
            match shape with
            | Rectangle r -> Calculators.rectangle r
            | Circle c -> Calculators.circle c
            | Ellipse e -> Calculators.ellipse e


module ShapeFactory =
    let ellipse semiMajorAxis semiMinorAxis =
        match PositiveFloat.create semiMajorAxis, PositiveFloat.create semiMinorAxis with
        | Ok semiMajorAxis, Ok semiMinorAxis ->
            Ok(
                Ellipse
                    { SemiMajorAxis = semiMajorAxis
                      SemiMinorAxis = semiMinorAxis }
            )
        | Error _, Ok _ -> Error "Semi-major axis must be >= 0"
        | Ok _, Error _ -> Error "Semi-minor axis must be >= 0"
        | Error _, Error _ -> Error "Semi-major and semi-minor axes must be >= 0"


    let circle radius =
        match PositiveFloat.create radius with
        | Ok radius -> Ok(Circle { Radius = radius })
        | Error _ -> Error "Radius must be >= 0"


    let rectangle width height =
        match PositiveFloat.create width, PositiveFloat.create height with
        | Ok width, Ok height -> Ok(Rectangle { Width = width; Height = height })
        | Error _, Ok _ -> Error "Width must be >= 0"
        | Ok _, Error _ -> Error "Height must be >= 0"
        | Error _, Error _ -> Error "Width and height must be >= 0"



module Result =
    // helper function for executing an action based on the both Ok and Error result of a Result
    let iterWithError okAction errorAction result =
        match result with
        | Ok value -> okAction value |> ignore
        | Error err -> errorAction err |> ignore

let circleArea () =
    ShapeFactory.circle 1
    |> Result.map AreaCalculator.calculate
    |> Result.iterWithError (fun (Area area) -> printfn $"Circle area: %f{PositiveFloat.value area}") (fun err ->
        printfn $"Error: %s{err}")

let rectangleArea () =
    ShapeFactory.rectangle 1 2
    |> Result.map AreaCalculator.calculate
    |> Result.iterWithError (fun (Area area) -> printfn $"Rectangle area: %f{PositiveFloat.value area}") (fun err ->
        printfn $"Error: %s{err}")

let ellipseArea () =
    ShapeFactory.ellipse 1 2
    |> Result.map AreaCalculator.calculate
    |> Result.iterWithError (fun (Area area) -> printfn $"Ellipse area: %f{PositiveFloat.value area}") (fun err ->
        printfn $"Error: %s{err}")

let circleAreaWithError () =
    ShapeFactory.circle -1
    |> Result.map AreaCalculator.calculate
    |> Result.iterWithError (fun (Area area) -> printfn $"Circle area: %f{PositiveFloat.value area}") (fun err ->
        printfn $"Error: %s{err}")

circleArea ()
rectangleArea ()
ellipseArea ()
circleAreaWithError ()
