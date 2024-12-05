open System

let readTabDelimitedFile (filename: string) =
    seq {
        use reader = new System.IO.StreamReader(filename)
        while not reader.EndOfStream do
            let line = reader.ReadLine()
            let parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            yield parts |> Array.map int
    }

let data = readTabDelimitedFile("test.txt") |> Seq.toList

let SafeLevels = 
    data    
    |> List.filter (fun line -> line |> Array.distinct |> Array.length = line.Length) // Filter out arrays with duplicates
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
    
let unSafeLevels = data |> List.filter (fun line -> not (SafeLevels |> List.contains line)) // Filter out safe levels

//NB: This is easy to read compared to the above! Breaking up your functions is easy on the eyes & brain ;)

// let hasDuplicates line =
//     line |> Array.distinct |> Array.length <> line.Length

let isStrictlyIncreasing line =
    line |> Array.pairwise |> Array.forall (fun (a, b) -> b > a)

let isStrictlyDecreasing line =
    line |> Array.pairwise |> Array.forall (fun (a, b) -> b < a)

let isBetween1And3 line =
    line
    |> Array.pairwise
    |> Array.forall (fun (a, b) -> abs(b - a) <= 3)

// -> Wrong!!!! Come back to this later
// Remove out of sequence elements or remove duplicates, it can only be one of the two
// let removeFirstOutOfSequenceOrDuplicate (line: int[]) =
//     let rec findOutOfSequenceIndex i =
//         if i >= line.Length - 1 then -1
//         elif line.[i] > line.[i + 1] then i
//         elif line.[i] < line.[i - 1] then i
//         else findOutOfSequenceIndex (i + 1)
    
//     let findFirstDuplicateIndex (line: int[]) =
//         let rec loop i seen =
//             if i >= line.Length then None
//             elif Set.contains line.[i] seen then Some i
//             else loop (i + 1) (Set.add line.[i] seen)
//         loop 0 Set.empty
    
//     let index = findOutOfSequenceIndex 1
//     if index <> -1 then // Remove out of sequence element
//         Array.append 
//             (Array.sub line 0 index) 
//             (Array.sub line (index + 1) (line.Length - index - 1))
//     else
//         let duplicateIndex = findFirstDuplicateIndex line
//         match duplicateIndex with
//         | None -> line // No out of sequence or duplicate element
//         | Some idx ->
//             Array.append 
//                 (Array.sub line 0 idx) 
//                 (Array.sub line (idx + 1) (line.Length - idx - 1))
            
// let DampenerSafeLevels = 
//     unSafeLevels
//     |> List.map removeFirstOutOfSequenceOrDuplicate  
//     |> List.filter (fun line -> 
//         isStrictlyIncreasing line || isStrictlyDecreasing line)
//     |> List.filter isBetween1And3

// :(
let tryRemoveOneElement (line: int[]) =
    [0..line.Length-1] //
    |> List.exists (fun skipIndex ->
        let newLine = 
            line 
            |> Array.mapi (fun i x -> (i, x)) 
            |> Array.filter (fun (i, _) -> i <> skipIndex) 
            |> Array.map snd
        
        // Check if this new sequence (with one element removed) is safe
        let hasNoDuplicates = newLine |> Array.distinct |> Array.length = newLine.Length
        let isValid = 
            hasNoDuplicates && 
            (isStrictlyIncreasing newLine || isStrictlyDecreasing newLine) &&
            isBetween1And3 newLine
        isValid)

// Check which unsafe levels can be made safe with the dampener
let DampenerSafeLevels = 
    unSafeLevels
    |> List.filter tryRemoveOneElement

let totalSafeLevels = SafeLevels.Length + DampenerSafeLevels.Length

// Debugging
printfn "\nLevels made safe by dampener:"
for line in DampenerSafeLevels do
    printfn "%A" line

printfn "\n----------------"
printfn $"Total safe levels: {totalSafeLevels}"