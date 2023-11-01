module TypeDrivenDevelopmentFSharp.Implementations.ShapeFactory

open TypeDrivenDevelopmentFSharp.Types

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

