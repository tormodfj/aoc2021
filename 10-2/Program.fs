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

let isCorrupted = corruptedChar >> Option.isSome

let missingChars (input:char list) : char list =
    let rec findRemainder (acc:char list) (input:char list) : char list =
        match (acc,input) with
        | ([],next::itail) -> findRemainder (next::[]) itail
        | (prev::atail, next::itail) when isOpening next -> findRemainder (next::prev::atail) itail
        | (prev::atail, next::itail) when isLegalClosing prev next -> findRemainder atail itail
        | (remainder,[]) -> remainder
        | _ -> failwith "Unrecognized input"
    let inverse (c:char) =
        match c with
        | '(' -> ')'
        | '[' -> ']'
        | '{' -> '}'
        | '<' -> '>'
        | _ -> failwith "Unrecognized input"

    input
    |> findRemainder []
    |> List.map inverse

let score (cs:char list) =
    let value (c:char) =
        match c with
        | ')' -> 1L
        | ']' -> 2L
        | '}' -> 3L
        | '>' -> 4L
        | _ -> 0L
    cs
    |> List.fold (fun s c -> (s * 5L) + (value c)) 0L

File.ReadLines "input"
|> Seq.map List.ofSeq
|> Seq.filter (isCorrupted >> not)
|> Array.ofSeq
|> Array.map missingChars
|> Array.map score
|> Array.sort
|> (fun scores -> scores[scores.Length / 2])
|> printfn "%i"
