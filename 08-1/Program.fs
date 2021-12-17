open System
open System.IO

type Line = { Input: string array; Output:string array }

let parseLine (input:string) =
    let parts = input.Split([| " | " |], StringSplitOptions.None)
    let input = parts[0].Split(' ')
    let output = parts[1].Split(' ')
    { Input = input; Output = output }

let isOneFourSevenOrEight (digit:string) =
    match digit.Length with
    | 2 | 3 | 4 | 7 -> true
    | _ -> false

let countOutput predicate line =
    line.Output
    |> Array.filter predicate
    |> Array.length

File.ReadLines "input"
|> Seq.map parseLine
|> Seq.map (countOutput isOneFourSevenOrEight)
|> Seq.sum
|> printfn "%i"
