open System.IO

File.ReadLines "input"
|> Seq.map int
|> Seq.pairwise
|> Seq.filter (fun (x,y) -> x < y)
|> Seq.length
|> printf "%i"
