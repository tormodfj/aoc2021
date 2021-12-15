open System.IO

type Octo = { Energy:int; Flashed:bool; FlashCount:int; Neighbours:int list }

let charToInt (c:char) = (int c) - (int '0')
let charToOcto (c:char) = { Energy = charToInt c; Flashed = false; FlashCount = 0; Neighbours = [] }
let mapNeighbours (i:int) octo =
    let neighbours = 
        seq {
            yield! seq { i-10; i+10 }
            if (i % 10 <> 0) then yield! seq { i-11; i-1; i+9 }
            if (i % 10 <> 9) then yield! seq { i-9; i+1; i+11 }
        }
        |> Seq.filter (fun i -> i >= 0 && i < 100)
        |> List.ofSeq
    { octo with Neighbours = neighbours }

let octos =
    File.ReadLines "input"
    |> Seq.collect (fun line -> line |> Seq.map charToOcto)
    |> Array.ofSeq
    |> Array.mapi mapNeighbours

let rec increaseEnergy i =
    let before = octos[i]
    let after = { before with Energy = before.Energy + 1 }
    octos[i] <- after

    match after with
    | { Energy = 10; Flashed = false} -> flash i
    | _ -> ()
    
and flash i =
    let before = octos[i]
    let after = { before with Flashed = true }
    octos[i] <- after
    
    match before with
    | { Flashed = false } -> before.Neighbours |> List.iter increaseEnergy
    | _ -> ()

let countAndReset i =
    let before = octos[i]
    let after =
        match before with
        | { Flashed = true} -> { before with Energy = 0; Flashed = false; FlashCount = before.FlashCount + 1 }
        | _ -> before
    octos[i] <- after

let step () =
    [0..99] |> List.iter increaseEnergy
    [0..99] |> List.iter countAndReset

for _ in 1..100 do
    step()

octos
|> Array.sumBy (fun o -> o.FlashCount)
|> printfn "%i"
