open System
open System.IO

type BitCriteria = (char*int) list -> char

let rec findRating (bitCriteria:BitCriteria) (bitIndex:int) (input:string list) =
    let bitToKeep (values:string list) = 
        values
        |> List.map (fun v -> v.[bitIndex])
        |> List.countBy id
        |> bitCriteria

    let filterValues (values:string list) =
        let bit = values |> bitToKeep
        values
        |> List.filter (fun s -> s.[bitIndex] = bit)

    match input with
    | result::[] -> result
    | _ -> findRating bitCriteria (bitIndex+1) (filterValues input)

let o2Criteria:BitCriteria = List.sortByDescending (fun (c,n) -> (n,c)) >> List.head >> fun(c,_) -> c
let co2Criteria:BitCriteria = List.sortBy (fun (c,n) -> (n,c)) >> List.head >> fun(c,_) -> c

let binaryToInt str = Convert.ToInt32(str, 2)
let o2RatingAnalyzer = findRating o2Criteria 0 >> binaryToInt
let co2RatingAnalyzer = findRating co2Criteria 0 >> binaryToInt

let report = File.ReadLines "input" |> List.ofSeq
let o2 = report |> o2RatingAnalyzer
let co2 = report |> co2RatingAnalyzer

printfn "%i" (o2*co2)
