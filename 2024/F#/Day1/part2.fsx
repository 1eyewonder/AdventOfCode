open System
open System.IO

[<return: Struct>]
let (|Int|_|) (s: string) =
  match Int32.TryParse(s) with
  | true, i -> ValueSome i
  | _ -> ValueNone

let tryGetLineItems (line: string) =
  let items = line.Split(' ') |> Array.filter (String.IsNullOrEmpty >> not)

  match items with
  | [| leftListItem; rightListItem |] ->
    match leftListItem, rightListItem with
    | Int l, Int r -> Some(l, r)
    | _ -> None
  | _ -> None

let leftList, rightList =
  File.ReadLines("data.txt")
  |> Seq.choose tryGetLineItems
  |> Seq.toList
  |> List.unzip

let rightMap = rightList |> List.countBy id |> Map.ofList

leftList
|> List.fold
  (fun total i ->
    match Map.tryFind i rightMap with
    | Some count -> (i * count) + total
    | None -> total)
  0
|> printfn "Answer: %A"
