open System.IO

type Point = { Height:int; Neighbours:int list }

let charToInt (c:char) = (int c) - (int '0')
let charToPoint (c:char) = { Height = charToInt c; Neighbours = [] }
let riskLevel (point:Point) = point.Height + 1
let mapNeighbours (i:int) (point:Point) =
    let neighbours = 
        seq {
            yield! seq { i-100; i+100 }
            if (i % 100 <> 0) then yield i-1
            if (i % 100 <> 99) then yield i+1
        }
        |> Seq.filter (fun i -> i >= 0 && i < 10000)
        |> List.ofSeq
    { point with Neighbours = neighbours }
let isLowPoint (allPoints:Point array) (point:Point) =
    point.Neighbours
    |> List.map (Array.get allPoints)
    |> List.forall (fun n -> n.Height > point.Height)

let points =
    File.ReadLines "input"
    |> Seq.collect id
    |> Seq.map charToPoint
    |> Array.ofSeq
    |> Array.mapi mapNeighbours

points
|> Array.filter (isLowPoint points)
|> Array.map riskLevel
|> Array.sum
|> printfn "%A"
