module TypeDrivenDevelopmentFSharp.Implementations.AreaCalculator

open TypeDrivenDevelopmentFSharp.Types


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

