let readFile (filename: string) =
    seq {
        use reader = new System.IO.StreamReader(filename)
        while not reader.EndOfStream do
            let line = reader.ReadLine()
            yield line.ToCharArray()
    }

let toCharArray2D (seq: seq<char[]>) =
    seq |> Array.ofSeq

let data = readFile("input.txt") |> toCharArray2D

// Debugging
// data |> Array.iter (fun line -> printfn "%A" line)

//Need to label the directions on the grid (data) so i can visualize it

// M M M S X X M A S M
// M S A M X M S M S A
// A M X S X M A A M M
// M S A M A S M S M X

let grid = [|
    [| 'M'; 'M'; 'M'; 'S'; 'X'; 'X'; 'M'; 'A'; 'S'; 'M' |]
    [| 'M'; 'S'; 'A'; 'M'; 'X'; 'M'; 'S'; 'M'; 'S'; 'A' |]
    [| 'A'; 'M'; 'X'; 'S'; 'X'; 'M'; 'A'; 'A'; 'M'; 'M' |]
    [| 'M'; 'S'; 'A'; 'M'; 'A'; 'S'; 'M'; 'S'; 'M'; 'X' |]
|]

//> Wont take credit for this, I found this solution on the internet: https://exercism.org/tracks/fsharp/exercises/word-search/solutions

// Directions represented as (dx, dy):
// (row, column):
// (0, 1): Move right (increase column index).
// (1, 0): Move down (increase row index).
// (1, 1): Move down-right (increase both row and column indexes).
// (1, -1): Move down-left (increase row index, decrease column index).
// (0, -1): Move left (decrease column index).
// (-1, 0): Move up (decrease row index).
// (-1, -1): Move up-left (decrease both row and column indexes).
// (-1, 1): Move up-right (decrease row index, increase column index).
let directions = [ (0, 1); (1, 0); (1, 1); (1, -1); (0, -1); (-1, 0); (-1, -1); (-1, 1) ]

// Check if the current cell is valid
let isValid (grid: char[][]) (x: int) (y: int) =
    x >= 0 && y >= 0 && x < grid.Length && y < grid.[0].Length

//Print Is Valid
// let IsvalidResult = isValid data 0 0
// printfn "%A" IsvalidResult

let searchWord (grid: char[][]) (word: string) (startX: int) (startY: int) (dirX: int) (dirY: int) =
    let rec loop i x y =
        if i = word.Length then true 
        elif not (isValid grid x y) || grid.[x].[y] <> word.[i] then false
        else loop (i + 1) (x + dirX) (y + dirY)
    loop 0 startX startY 

//Print Search Word
let searchWordResult = searchWord data "XMAS" 0 0 62 1
printfn "%A" searchWordResult

let countWord (grid: char[][]) (word: string) =
    let mutable count = 0 //Must change
    for x in 0 .. grid.Length - 1 do
        for y in 0 .. grid.[0].Length - 1 do
            for (dirX, dirY) in directions do
                if searchWord grid word x y dirX dirY then
                    count <- count + 1
    count

let word = "XMAS"
let result = countWord grid word

printfn "%A" result