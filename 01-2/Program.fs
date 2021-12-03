open System.IO

File.ReadLines "input"
|> Seq.map int
|> Seq.windowed 3
|> Seq.map Seq.sum
|> Seq.pairwise
|> Seq.filter (fun (x,y) -> x < y)
|> Seq.length
|> printfn "%i"
