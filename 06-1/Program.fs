open System.IO

let input = File.ReadAllText "input"
let initial = input.Split(',') |> List.ofArray |> List.map int

let evolve (state:int list):int list =
    let rec doEvolve prev next =
        match prev with
        | [] -> next
        | 0::tail -> doEvolve tail (6::8::next)
        | n::tail -> doEvolve tail (n-1::next)
    doEvolve state []

let rec evolveN n state = 
    match n with
    | 0 -> state
    | _  -> evolveN (n-1) (evolve state)

initial
|> evolveN 80
|> List.length
|> printfn "%i"
