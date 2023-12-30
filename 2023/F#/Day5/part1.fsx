open System
open System.IO
open System.Text.RegularExpressions

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let (|SplitBy|_|) (x: string) (text: string) =
    text.Split(x, StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> Some

let data = 
    File.ReadAllLines("data.txt")
    |> Array.filter ((<>) String.Empty)
    |> Array.indexed
    |> Array.toList

[<Literal>]
let aToBMap = @"([a-z,A-Z]*)-to-([a-z,A-Z]*)\s{1}map:"

let tryGetSeeds data =
    match List.tryHead data with
    | Some (SplitBy "seeds:" [SplitBy " " seeds]) -> 
        List.map int64 seeds |> Some
    | _ -> None

let getMaps data =
    data
    |> List.choose (fun (index, text) ->
        match text with
        | ParseRegex aToBMap [a; b] -> Some (index, (a, b))
        | _ -> None
    )

data |> List.map snd |> tryGetSeeds |> printfn "Seeds: %A"

let mapData = getMaps data

let groupMapData mapData =
    mapData
    |> List.pairwise
    |> List.map (fun ((x, k1), (y, _)) ->
        let mapData =
            List.splitAt x data 
            |> snd
            |> List.splitAt (y - x)
            |> fst
            |> List.tail
            |> List.map snd
        
        k1, mapData
    )
    |> List.append (
        let last = List.rev mapData |> List.head
        List.singleton ((snd last), (List.splitAt ((fst last) + 1) data |> snd |> List.map snd))
    )

let parseMapData (source, dest) (lines: string array) =
    lines
    |> Array.Parallel.choose (function
        | SplitBy " " [dNum; sNum; range] ->
            let s = int64 sNum
            let d = int64 dNum
            let r = int64 range
            Some ((source, dest), {| Source = s; Destination = d; Range = r |})
        | _ -> None
    )

groupMapData mapData
|> List.map (fun (a, b) -> parseMapData a (List.toArray b))
|> printfn "%A"