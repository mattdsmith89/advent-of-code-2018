open Microsoft.FSharp.Core
open System
#load "Utils.fs"
open Utils

let input = Utils.readLines "AdventOfCode/input/day02.txt"
//let input = ["abcdef";"bababc";"abbcde";"abcccd";"aabcdd";"abcdee";"ababab"]
//let input = ["abcde";"fghij";"klmno";"pqrst";"fguij";"axcye";"wvxyz"]

let partAFolder acc item =
    let (currDoubles, currTriples) = acc
    let chars = Seq.toList item |> Seq.countBy id |> Seq.toList
    let hasDoubles = chars |> Seq.exists (fun (_, count) -> count = 2)
    let hasTriples = chars |> Seq.exists (fun (_, count) -> count = 3)
    let newDoubles = currDoubles + if hasDoubles then 1 else 0 
    let newTriples = currTriples + if hasTriples then 1 else 0
    (newDoubles, newTriples)

let solveA ids = 
    Seq.fold partAFolder (0,0) ids

let computeDistance source candidate =
    Seq.zip source candidate
    |> Seq.filter (fun (x, y) -> x <> y)
    |> Seq.length

let solveB ids =
    let matches = 
        ids 
        |> Seq.choose (fun id -> 
            match id with
                | id when Seq.length (ids |> Seq.filter (fun x -> (computeDistance id x) = 1)) = 1 -> Some(id)
                | _ -> None)

    let result1 = Seq.head matches
    let result2 = Seq.skip 1 matches |> Seq.head
    Seq.zip result1 result2 |> Seq.filter (fun (x, y) -> x = y) |> Seq.map (fun (x,_) -> x) |> Seq.toArray  
                   


let (doubleCount, tripleCount) = solveA input
let result = solveB input |> String

let checksum = doubleCount * tripleCount
printfn "Checksum is %i * %i = %i" doubleCount tripleCount checksum
printfn "Part b ans %s" result