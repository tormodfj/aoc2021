open System
open System.IO

type Line = { Input: string array; Output:string array }
type WireMapper = char -> char

let constructWireMapper (line:Line) : WireMapper = 
    // Counts occurrence of character across input patterns
    let count (c:char) =
        line.Input
        |> Array.filter (fun s -> s.Contains(c))
        |> Array.length

    // These are the patterns we can identify by length
    let patterns = 
        Map [
            (1, line.Input |> Array.find (fun d -> d.Length = 2) |> Set.ofSeq)
            (4, line.Input |> Array.find (fun d -> d.Length = 4) |> Set.ofSeq)
            (7, line.Input |> Array.find (fun d -> d.Length = 3) |> Set.ofSeq)
            (8, line.Input |> Array.find (fun d -> d.Length = 7) |> Set.ofSeq)
        ]

    // 'a' is the segment that exists in 7 but not in 1
    let a = Set.difference patterns[7] patterns[1] |> Seq.head

    // 'c' and 'f' segments can be identified from the 1 pattern
    // ('c' segments are fewer in total than 'f' segments)
    let (c,f) = 
        patterns[1]
        |> Array.ofSeq
        |> Array.sortBy count
        |> fun cs -> (cs[0], cs[1])

    // 'b' and 'd' segments can be identified by subtracting the 1 pattern from the 4 pattern
    // ('b' segments are fewer in total than 'd' segments)
    let bd = Set.difference patterns[4] patterns[1] |> Array.ofSeq
    let (b,d) =
        bd
        |> Array.sortBy count
        |> fun cs -> (cs[0], cs[1])

    // 'e' and 'g' segment can be identified by subtracting other segments from the 8 pattern
    // ('e' segments are fewer in total than 'g' segments)
    let eg = Set.difference patterns[8] (Set.ofList [a;b;c;d;f]) |> Array.ofSeq
    let (e,g) =
        eg
        |> Array.sortBy count
        |> fun cs -> (cs[0], cs[1])

    // Gather our findings
    let mapping =
        Map [
            (a, 'a')
            (b, 'b')
            (c, 'c')
            (d, 'd')
            (e, 'e')
            (f, 'f')
            (g, 'g')
        ]
    // Return our mapping function
    fun c -> mapping[c]

let segmentsToDigit (segment:char seq):char =
    let map input =
        match input with
        | "abcefg" -> '0'
        | "cf" -> '1'
        | "acdeg" -> '2'
        | "acdfg" -> '3'
        | "bcdf" -> '4'
        | "abdfg" -> '5'
        | "abdefg" -> '6'
        | "acf" -> '7'
        | "abcdefg" -> '8'
        | "abcdfg" -> '9'
        | _ -> failwithf "Unrecognized segment"

    segment
    |> Seq.sort
    |> Array.ofSeq
    |> String
    |> map

let calculateOutputValue line : int = 
    let wireMapper = 
        line
        |> constructWireMapper
    let getDigitChar input =
        input
        |> Seq.map wireMapper
        |> segmentsToDigit

    line.Output
    |> Array.map getDigitChar
    |> String
    |> int

let parseLine (input:string) =
    let parts = input.Split([| " | " |], StringSplitOptions.None)
    let input = parts[0].Split(' ')
    let output = parts[1].Split(' ')
    { Input = input; Output = output }

File.ReadLines "input"
|> Seq.map parseLine
|> Seq.map calculateOutputValue
|> Seq.sum
|> printfn "%i"
