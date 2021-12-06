open System
open System.IO

type Board = { Numbers: Set<int>; Combinations: Set<int> list; Drawn: int list }

let parseBoard (input:string):Board =
    let parseLine (line:string) =
        line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map int
    let lines = 
        input.Split('\n') 
        |> List.ofArray
        |> List.map parseLine
    
    let numbers = lines |> List.concat |> Set.ofList
    let rows = lines |> List.map Set.ofList
    let cols = lines |> List.transpose |> List.map Set.ofList
    
    { Numbers = numbers; Combinations = List.concat [|rows; cols|]; Drawn = [] }
    
let isWon (board:Board):bool =
    board.Combinations
    |> List.exists (fun set -> board.Drawn |> Set.ofList |> Set.isSubset set )

let rec play (numbers:int list) (board:Board) :Board =
    match numbers with
    | [] -> board
    | head::tail ->
        match isWon board with
        | true -> board
        | false -> play tail { board with Drawn = head::board.Drawn }

let score (board:Board):int =
    let sumOfUnmarked =
        board.Drawn
        |> Set.ofList
        |> Set.difference board.Numbers
        |> Seq.sum
    sumOfUnmarked * board.Drawn.Head

let input = File.ReadAllText "input"
let parts = input.Split('\n', 2)

let numbers = 
    parts.[0].Split(',') 
    |> List.ofArray 
    |> List.map int

let boards = 
    parts.[1].Trim().Split("\r\n\r\n") 
    |> List.ofArray 
    |> List.map parseBoard

boards
|> List.map (play numbers)
|> List.sortBy (fun board -> board.Drawn.Length)
|> List.head
|> score
|> printfn "%i"
