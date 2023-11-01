// For more information see https://aka.ms/fsharp-console-apps

open TypeDrivenDevelopmentFSharp.Implementations
open TypeDrivenDevelopmentFSharp.Types

module Result =
    let tap okAction result =
        match result with
        | Ok value -> okAction value |> ignore
        | Error _ -> ()

        result

    let tapError errorAction result =
        match result with
        | Ok _ -> ()
        | Error err -> errorAction err |> ignore

        result

let circleArea () =
    ShapeFactory.circle 1
    |> Result.map AreaCalculator.calculate
    |> Result.tap (fun (Area area) -> printfn $"Circle area: %f{PositiveFloat.value area}")
    |> Result.tapError (fun err -> printfn $"Error: %s{err}")

let rectangleArea () =
    ShapeFactory.rectangle 1 2
    |> Result.map AreaCalculator.calculate
    |> Result.tap (fun (Area area) -> printfn $"Rectangle area: %f{PositiveFloat.value area}")
    |> Result.tapError (fun err -> printfn $"Error: %s{err}")

let ellipseArea () =
    ShapeFactory.ellipse 1 2
    |> Result.map AreaCalculator.calculate
    |> Result.tap (fun (Area area) -> printfn $"Ellipse area: %f{PositiveFloat.value area}")
    |> Result.tapError (fun err -> printfn $"Error: %s{err}")

let circleAreaWithError () =
    ShapeFactory.circle -1
    |> Result.map AreaCalculator.calculate
    |> Result.tap (fun (Area area) -> printfn $"Circle area: %f{PositiveFloat.value area}")
    |> Result.tapError (fun err -> printfn $"Error: %s{err}")

circleArea () |> ignore
rectangleArea () |> ignore
ellipseArea () |> ignore
circleAreaWithError () |> ignore
