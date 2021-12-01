open System
open System.IO

File.ReadAllLines("input")
|> Array.map Int32.Parse
|> Array.pairwise
|> Array.filter (fun (x,y) -> x < y)
|> Array.length
|> printf "%i"
