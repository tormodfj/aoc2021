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

let executeCommand (horiz, depth) command =
    match command with
    | Forward(d) -> (horiz + d, depth)
    | Up(d) -> (horiz, depth - d)
    | Down(d) -> (horiz, depth + d)

File.ReadLines "input"
|> Seq.map parseCommand
|> Seq.fold executeCommand (0, 0)
|> fun(horiz, depth) -> horiz * depth 
|> printf "%i"
