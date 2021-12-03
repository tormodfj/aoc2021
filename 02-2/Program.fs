open System
open System.IO

type Command =
    | Forward of int
    | Up of int
    | Down of int

let parseCommand (input:string) =
    let command = input.Split(' ') |> List.ofArray
    match command with
    | "forward"::d::[] -> Forward(int d)
    | "up"::d::[] -> Up(int d)
    | "down"::d::[] -> Down(int d)
    | _ -> failwith "unrecognized command"

let executeCommand (horiz, depth, aim) command =
    match command with
    | Forward(d) -> (horiz + d, depth + d*aim, aim)
    | Up(d) -> (horiz, depth, aim - d)
    | Down(d) -> (horiz, depth, aim + d)

File.ReadLines "input"
|> Seq.map parseCommand
|> Seq.fold executeCommand (0, 0, 0)
|> fun(horiz, depth, _) -> horiz * depth 
|> printfn "%i"
