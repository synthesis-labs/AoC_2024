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

// let levels = 
//     data
//     |> List.filter (fun line -> line |> Array.distinct |> Array.length = line.Length) // Filter out arrays with duplicates    
//     |> List.map (fun line ->         
//         let decrementing = 
//             line
//             |> Array.pairwise
//             |> Array.forall (fun (a, b) -> b < a)
//         if decrementing then Some line else None)       
//     |> List.choose id
//     |> List.filter (fun line -> 
//         line
//         |> Array.pairwise
//         |> Array.forall (fun (a, b) ->  abs(b - a) <= 3))  

// let levels = 
//     data
//     |> List.filter (fun line -> line |> Array.distinct |> Array.length = line.Length) // Filter out arrays with duplicates    
//     |> List.map (fun line ->         
//         let incrementing = 
//             line
//             |> Array.pairwise
//             |> Array.forall (fun (a, b) -> b > a)
//         if incrementing then Some line else None)       
//     |> List.choose id
//     |> List.filter (fun line -> 
//         line
//         |> Array.pairwise
//         |> Array.forall (fun (a, b) -> abs(b - a) <= 3)) 

// Array.pairwise -> Returns an array of each element in the input array and its predecessor, with the exception of the first element which is only returned as the predecessor of the second element.
// Array.forall -> Returns true if all elements in the array satisfy the given predicate, otherwise false.

// Remove levels that are not increasing or decreasing and any duplicate levels
let levels = 
    data    
    |> List.filter (fun line -> line |> Array.distinct |> Array.length = line.Length)
    |> List.map (fun line ->         
        let decrementing = 
            line                       
            |> Array.pairwise 
            |> Array.forall (fun (a, b) -> b < a)                        
        let incrementing = 
            line            
            |> Array.pairwise
            |> Array.forall (fun (a, b) -> b > a)
        if incrementing || decrementing then Some line else None)       
    |> List.choose id        
    |> List.filter (fun line -> 
        line
        |> Array.pairwise
        |> Array.forall (fun (a, b) ->  abs(b - a) <= 3))    

// Debugging
for line in levels do
     printfn "%A" line

printfn "----------------"
printfn $"Total Safe Reports: {levels.Length}"