open System.IO

type Polymer = char list
type Rule = { Pair:(char*char); Insertion:char }
type RuleBook = Map<(char*char), char>

let parseTemplate (input:string) : Polymer = 
    input
    |> List.ofSeq
let parseRules (input:string seq) : RuleBook =
    input
    |> Seq.map (fun s -> ((s[0],s[1]), s[6]))
    |> Map.ofSeq

let step (rules:RuleBook) (input:Polymer) : Polymer =
    let rec applyRule (acc:Polymer) (pol:Polymer) =
        match pol with
        | a::b::tail when rules.ContainsKey (a,b) -> applyRule (rules[a,b]::a::acc) (b::tail)
        | a::tail -> applyRule (a::acc) tail
        | [] -> acc |> List.rev
    applyRule [] input

let getMinMax (input:Polymer) : (int*int) =
    let counts = input |> List.countBy id
    let (_, min) = counts |> List.minBy (fun(_,n) -> n)
    let (_, max) = counts |> List.maxBy (fun(_,n) -> n)
    (min, max)

let input = File.ReadLines "input"

let template =
    input
    |> Seq.head
    |> parseTemplate
let ruleBook =
    input
    |> Seq.skip 2
    |> parseRules

[1..10]
|> List.fold (fun p _ -> (step ruleBook p)) template
|> getMinMax
|> (fun (min,max) -> max-min)
|> printfn "%i"
