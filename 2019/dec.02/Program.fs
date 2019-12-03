// Learn more about F# at http://fsharp.org

open System

let readlines filePath = System.IO.File.ReadLines(filePath)
let loadMemory (input : string) = input.Split ',' |> Seq.map int |> Seq.toList
let stringify (input : list<int>) = input |> Seq.map string |> String.concat ","

let addOp instPtr (memory : list<int>) = 
    match memory.[(instPtr+1)..] with
    | p1::p2::p3::_ -> memory |> List.mapi (fun adrs x -> if adrs = p3 then Seq.item p1 memory + Seq.item p2 memory else x)
    | _ -> memory

let multOp instPtr (memory : list<int>) = 
    match memory.[(instPtr+1)..] with
    | p1::p2::p3::_ -> memory |> List.mapi (fun adrs x -> if adrs = p3 then Seq.item p1 memory * Seq.item p2 memory else x)
    | _ -> memory

let rec intcode instPtr memory  =
    match Seq.item instPtr memory with
    | 1 -> addOp instPtr memory |> intcode (instPtr + 4)
    | 2 -> multOp instPtr memory |> intcode (instPtr + 4) 
    | 99 -> memory
    | _ -> memory

let setInput noun verb (memory : list<int>) = memory.[0]::noun::verb::memory.[3..]

let determineInputs output memory  =
    let nouns = [0..99]
    let verbs = [0..99]
    List.allPairs nouns verbs 
    |> List.map (fun (noun, verb) -> noun, verb, setInput noun verb memory |> intcode 0 |> Seq.item 0)
    |> List.find (fun (noun, verb, genOut) -> genOut = output)
    |> (fun (noun, verb, _) -> 100 * noun + verb)

let runProgramPart1 x =
    //x |> loadMemory |> intcode 0 |> stringify |> printfn "%s"
    x |> loadMemory |> setInput 12 2 |> intcode 0 |> Seq.item 0 |> printfn "position 0 = %i"

let runProgramPart2 x =
    x |> loadMemory |> determineInputs 19690720 |> printfn "nv = %i"


[<EntryPoint>]
let main argv =
    match argv with
    | x when x.Length > 0 -> 
        argv.[0]
        |> runProgramPart1
    | _ -> 
        readlines "input.txt" 
        |> Seq.item 0
        |> runProgramPart2

    0 // return an integer exit code
