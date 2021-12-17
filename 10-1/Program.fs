open System.IO

let isOpening (c:char) =
    match c with
    | '(' | '[' | '{' | '<' -> true
    | _ -> false
let isLegalClosing (prev:char) (next:char) =
    match (prev, next) with
    | ('(',')') | ('[',']') | ('{','}') | ('<','>') -> true
    | _ -> false

let corruptedChar (input:char list) : char option =
    let rec findCorruptedChar (acc:char list) (input:char list) : char option =
        match (acc,input) with
        | ([],next::itail) -> findCorruptedChar (next::[]) itail
        | (prev::atail, next::itail) when isOpening next -> findCorruptedChar (next::prev::atail) itail
        | (prev::atail, next::itail) when isLegalClosing prev next -> findCorruptedChar atail itail
        | (_, next::_) -> Some(next)
        | _ -> None
    findCorruptedChar [] input

let score (c:char) =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

File.ReadLines "input"
|> Seq.map List.ofSeq
|> Seq.choose corruptedChar
|> Seq.map score
|> Seq.sum
|> printfn "%i"
