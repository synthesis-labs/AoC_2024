open System

let readTabDelimitedFile (filename: string) =
    seq {
        use reader = new System.IO.StreamReader(filename)
        while not reader.EndOfStream do
            let line = reader.ReadLine()
            let parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            yield parts |> Array.map int
    }

let data = readTabDelimitedFile("input.txt") |> Seq.toList



// Debugging
// for line in levels do
//      printfn "%A" line

// printfn "----------------"
// printfn $"Total Safe Reports: {levels.Length}"