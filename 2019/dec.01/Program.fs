// Learn more about F# at http://fsharp.org

open System

let fuelAdderUpper mass = (mass / 3) - 2

let rec betterFuelAdderUpper mass =
    let fuelRequired = fuelAdderUpper mass
    match fuelRequired with
    | x when x > 0 -> betterFuelAdderUpper fuelRequired + fuelRequired
    | _ -> 0

let readlines filePath = System.IO.File.ReadLines(filePath)

[<EntryPoint>]
let main argv =
    match argv with
    | x when x.Length > 0 -> 
        argv.[0] 
        |> int
        |> betterFuelAdderUpper
        |> printfn "result = %i"
    | _ -> 
        readlines "input.txt" 
        |> Seq.sumBy (int >> betterFuelAdderUpper)
        |> printfn "result = %i"

    0 // return an integer exit code
