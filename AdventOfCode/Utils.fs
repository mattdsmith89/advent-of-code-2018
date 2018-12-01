namespace Utils

module Utils =
    open System.IO
    let readLines (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let readlinesInt (filePath:string) = readLines filePath |> Seq.map System.Int32.Parse