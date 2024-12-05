open System
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllText "test2.txt"

// use regx to find the following pattern: mul\((\d{1,3}),(\d{1,3})\) -> https://regex101.com/
let matches = Regex.Matches(input, @"mul\((\d{1,3}),(\d{1,3})\)")

// load the matches into an array of tuples
let data = 
    matches 
    |> Seq.cast<Match>
    |> Seq.map (fun m -> 
        let x = int m.Groups[1].Value
        let y = int m.Groups[2].Value
        (x, y))
    |> Seq.toArray

// Multiply each pair of x, y indexes and then sum them
let multiplyAndSum (data: (int * int)[]) =
    data
    |> Array.map (fun (x, y) -> x * y) // Multiply each pair of x, y indexes
    |> Array.sum // Sum the results

let result = multiplyAndSum data

printfn "Total Sum %d" result
