open System
open System.IO
open System.Text.RegularExpressions

[<AutoOpen>]
module Helpers =

  let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
      Some(List.tail [ for x in m.Groups -> x.Value ])
    else
      None

  let (|SplitBy|_|) (x: string) (text: string) =
    text.Split(x, StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> Some

[<RequireQualifiedAccess>]
module List =

  let splitBetween x y ls =
    List.splitAt (x + 1) ls |> snd |> List.splitAt (y - x - 1) |> fst

module Parsing =

  let tryGetSeeds data =
    match List.map snd data |> List.tryHead with
    | Some(SplitBy "seeds:" [ SplitBy " " seeds ]) -> List.map int64 seeds |> Some
    | _ -> None

  [<Literal>]
  let aToBMap = @"([a-z,A-Z]*)-to-([a-z,A-Z]*)\s{1}map:"

  let getMaps data =
    data
    |> List.choose (fun (index, text) ->
      match text with
      | ParseRegex aToBMap [ a; b ] -> Some(index, (a, b))
      | _ -> None)
    |> List.windowed 2
    |> List.map (fun a ->
      let x = fst a.Head + 1
      let y = fst a.Tail.Head
      let mapData = data |> List.splitBetween x y
      (snd a.Head), mapData)

let convertMapText (source, dest) (lines: string array) =
  lines
  |> Array.Parallel.choose (function
    | SplitBy " " [ dNum; sNum; range ] ->
      ((source, dest),
       {|
         Source = int64 sNum
         Destination = int64 dNum
         Range = int64 range
       |})
      |> Some
    | _ -> None)
  |> Array.fold
    (fun map (key, data) ->
      match Map.tryFind key map with
      | Some x -> Map.add key (data :: x) map
      | None -> Map.add key [ data ] map)
    Map.empty

let data =
  File.ReadAllLines("data.txt")
  |> Array.filter ((<>) String.Empty)
  |> Array.indexed
  |> Array.toList

let seeds = Parsing.tryGetSeeds data

let mapData =
  Parsing.getMaps data
  |> List.map (fun (a, b) ->
    let b = List.toArray b |> Array.map snd
    convertMapText a b)

printfn "%A" mapData
