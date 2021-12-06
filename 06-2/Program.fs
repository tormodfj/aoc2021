open System.IO

let input = File.ReadAllText "input"
let initial = 
    input.Split(',') 
    |> List.ofArray
    |> List.map int
    |> List.countBy id
    |> List.map (fun (k,i) -> (k, bigint i))
    |> Map.ofList

let evolve (state:Map<int,bigint>) =
    let getStateFor key =
        match Map.tryFind key state with
        | Some(v) -> v
        | None -> 0I
        
    Map [
        (0, getStateFor 1)
        (1, getStateFor 2)
        (2, getStateFor 3)
        (3, getStateFor 4)
        (4, getStateFor 5)
        (5, getStateFor 6)
        (6, getStateFor 7 + getStateFor 0)
        (7, getStateFor 8)
        (8, getStateFor 0)
    ]

let rec evolveN n state = 
    match n with
    | 0 -> state
    | _  -> evolveN (n-1) (evolve state)

initial
|> evolveN 256
|> Map.values
|> Seq.sum
|> printfn "%O"
