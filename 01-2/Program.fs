open System
open System.IO

File.ReadAllLines("input")
|> Seq.map Int32.Parse
|> Seq.windowed 3
|> Seq.map Seq.sum
|> Seq.pairwise
|> Seq.filter (fun (x,y) -> x < y)
|> Seq.length
|> printf "%i"
