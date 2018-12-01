#load "Utils.fs"
open Utils

let input = Utils.readlinesInt "AdventOfCode/input/day1.txt"
let solveA = Seq.sum
let solveB deltas =
    let cumulativeSums = 
        Seq.scan (+) 0 deltas
        |> Seq.tail
        |> Seq.toArray
    let setOfSums = Set.ofArray cumulativeSums
    let finalSum = Array.last cumulativeSums
    let rec iterate sums = 
        let newSums = (Array.map ((+) finalSum) sums)
        let firstMatch = Array.tryFind (fun i -> Set.contains i setOfSums) newSums
        match firstMatch with
        | Some x -> x
        | None -> iterate newSums
    iterate cumulativeSums    

printfn "Final freq is %i" <| solveA input
printfn "First repeated freq is %i" <| solveB input