// Types
open System

type RectangleType = { Width: float; Height: float }

type CircleType = { Radius: float }

type EllipseType =
    { SemiMajorAxis: float
      SemiMinorAxis: float }

type Shape =
    | Rectangle of RectangleType
    | Circle of CircleType
    | Ellipse of EllipseType

type Area = Area of float

type ValidationErrors =
    | NegativeValue of float
    | ZeroValue

type AreaCalculator = Shape -> Area

module AreaCalculator =
    module private Calculators =
        let rectangle r = Area(r.Width * r.Height)

        let circle c = Area(Math.PI * c.Radius * c.Radius)

        let ellipse e =
            Area(Math.PI * e.SemiMajorAxis * e.SemiMinorAxis)

    let calculate: AreaCalculator =
        fun shape ->
            match shape with
            | Rectangle r -> Calculators.rectangle r
            | Circle c -> Calculators.circle c
            | Ellipse e -> Calculators.ellipse e


module ShapeFactory = 
    let ellipse semiMajorAxis semiMinorAxis =
        // match arguments as `float` types to be non negative > 0
        match semiMajorAxis, semiMinorAxis with
        | semiMajorAxis, semiMinorAxis when semiMajorAxis > 0.0 && semiMinorAxis > 0.0 ->
            Ok (Ellipse { SemiMajorAxis = semiMajorAxis; SemiMinorAxis = semiMinorAxis })
        | semiMajorAxis, semiMinorAxis when semiMajorAxis <= 0.0 && semiMinorAxis <= 0.0 ->
            Error [NegativeValue semiMajorAxis; NegativeValue semiMinorAxis]
        | semiMajorAxis, _ when semiMajorAxis <= 0.0 ->
            Error [NegativeValue semiMajorAxis]
        | _, semiMinorAxis when semiMinorAxis <= 0.0 ->
            Error [NegativeValue semiMinorAxis]
        | _ -> failwith "impossible" // compiler doesn't know this is unreachable

    let circle radius =
        match radius with
        | radius when radius > 0.0 -> Ok (Circle { Radius = radius })
        | radius when radius <= 0.0 -> Error [NegativeValue radius]
        | _ -> failwith "impossible" // compiler doesn't know this is unreachable
        
    let rectangle width height =
        match width, height with
        | width, height when width > 0.0 && height > 0.0 ->
            Ok (Rectangle { Width = width; Height = height })
        | width, height when width <= 0.0 && height <= 0.0 ->
            Error [NegativeValue width; NegativeValue height]
        | width, _ when width <= 0.0 ->
            Error [NegativeValue width]
        | _, height when height <= 0.0 ->
            Error [NegativeValue height]
        | _ -> failwith "impossible" // compiler doesn't know this is unreachable


