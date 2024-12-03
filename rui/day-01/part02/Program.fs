module AoC_2024.Day01.Part02

open System

let readTabDelimitedFile (filename: string) =
    seq {
        use reader = new System.IO.StreamReader(filename)
        while not reader.EndOfStream do
            let line = reader.ReadLine()
            let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            yield (int parts[0], int parts[1])
    }

// let data = readTabDelimitedFile("input.txt") |> Seq.toList

// // Sort each index independently
// let sortedIndex1 = data |> List.map fst |> List.sort // Extract and sort the first values
// let sortedIndex2 = data |> List.map snd |> List.sort // Extract and sort the second values

// let dupChkData = 
//     sortedIndex1 
//     |> List.map (fun x -> 
//         let count = sortedIndex2 |> List.filter (fun y -> y = x) |> List.length
//         (x, count))
//     |> List.filter (fun (x, count) -> count > 0)

// let similarityChk = dupChkData |> List.map (fun (x, count) -> x * count)

// // Add the differences for each item to give a total sum
// let total = similarityChk |> List.sum  

// // Debugging
// for line in dupChkData do
//    printfn "%A" line

// printfn "-------------"
// printfn "%d"  total


let total = 
    readTabDelimitedFile("input.txt")
    |> Seq.toList
    |> fun lst -> 
        let sortedIndex1 = lst |> List.map fst |> List.sort
        let sortedIndex2 = lst |> List.map snd |> List.sort
        (sortedIndex1 , sortedIndex2)
    |> fun (sortedIndex1, sortedIndex2) -> 
        let dupChkData = 
            sortedIndex1 
            |> List.map (fun x -> 
                let count = sortedIndex2 |> List.filter (fun y -> y = x) |> List.length
                (x, count))
            |> List.filter (fun (x, count) -> count > 0)
        dupChkData
    |> List.map (fun (x, count) -> x * count)
    |> List.sum

printfn "%d"  total


