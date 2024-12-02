module AoC_2024.Day01

open System

let readTabDelimitedFile (filename: string) =
    seq {
        use reader = new System.IO.StreamReader(filename)
        while not reader.EndOfStream do
            let line = reader.ReadLine()
            let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            yield (int parts[0], int parts[1])
    }

let data = readTabDelimitedFile("input.txt") |> Seq.toList

// Sort each index independently
let sortedIndex1 = data |> List.map fst |> List.sort // Extract and sort the first values
let sortedIndex2 = data |> List.map snd |> List.sort // Extract and sort the second values

// Combine the sorted lists into tuples
let sortedData = List.zip sortedIndex1 sortedIndex2

// calculate the difference between each pair of consecutive values, ensuring that the difference is always positive
let differences = sortedData |> List.map (fun (a, b) -> abs(a - b))

//Add the differences for each item to give a total sum
let total = List.sum(differences)

// Debugging
// for line in differences do
//     printfn "%A" line

printfn "%d"  total

