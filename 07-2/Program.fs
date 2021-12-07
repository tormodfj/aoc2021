open System.IO

let input = File.ReadAllText "input"
let crabs = 
    input.Split(',')
    |> Array.map int
    |> Array.sort

let calculateFuel (pos:int) =
    // The sum of first n natural numbers is n(n+1)/2
    let distanceToFuel (dist:int) = dist*(dist+1)/2

    crabs
    |> Array.map ((-) pos)
    |> Array.map abs
    |> Array.map distanceToFuel
    |> Array.sum

{crabs[0]..crabs[crabs.Length-1]}
|> Seq.map calculateFuel
|> Seq.min
|> printfn "%i"
