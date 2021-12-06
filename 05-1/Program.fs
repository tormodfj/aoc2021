open System.IO
open System.Text.RegularExpressions

type Point = (int*int)
type Line = { Start:Point; End:Point }

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine input =
    match input with
    | Regex "(\d+),(\d+) -> (\d+),(\d+)" [x1;y1;x2;y2] -> { Start = (int x1, int y1); End = (int x2, int y2) }
    | _ -> failwith "unrecognized input"

let pointsFromLine { Start=(x1,y1); End=(x2,y2) } =  
    let (..) a b = if a < b then seq { a..b } else seq { a..(-1)..b }

    if x1=x2 then
        seq { for y in y1..y2 -> (x1,y) }
    else if y1=y2 then
        seq { for x in x1..x2 -> (x,y1) }
    else
        Seq.empty

let lines =
    File.ReadLines "input"
    |> Seq.map parseLine
    |> Seq.collect pointsFromLine
    |> Seq.countBy id
    |> Seq.filter (fun (_,n) -> n >= 2)
    |> Seq.length
    |> printfn "%i"
