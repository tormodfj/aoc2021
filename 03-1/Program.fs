open System
open System.IO

let report =
    File.ReadLines "input"
    |> Seq.map List.ofSeq
    |> List.transpose

let binaryGammaRate =
    report
    |> List.map (List.countBy id)
    |> List.map (List.maxBy (fun (_,n) -> n))
    |> List.map (fun (c,_) -> c)

let binaryEpsilonRate =
    binaryGammaRate
    |> List.map (fun c -> match c with |'0' -> '1'|'1' -> '0'|_ -> failwith "wtf")

let binaryToInt cs = Convert.ToInt32(cs |> Array.ofList |> String, 2)
let gammaRate = binaryGammaRate |> binaryToInt
let epsilonRate = binaryEpsilonRate |> binaryToInt

printfn "%i" (gammaRate*epsilonRate)
