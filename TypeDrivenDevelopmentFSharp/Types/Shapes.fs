namespace TypeDrivenDevelopmentFSharp.Types

open PositiveFloat

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


