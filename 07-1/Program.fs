open System.IO

let input = File.ReadAllText "input"
let crabs = 
    input.Split(',')
    |> Array.map int
    |> Array.sort

let calculateFuel (pos:int) =
    crabs
    |> Array.map ((-) pos)
    |> Array.map abs
    |> Array.sum

{crabs[0]..crabs[crabs.Length-1]}
|> Seq.map calculateFuel
|> Seq.min
|> printfn "%i"
