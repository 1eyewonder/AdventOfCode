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
  ||> fun left right -> List.sort left, List.sort right

(leftList, rightList)
||> List.fold2
  (fun total l r ->
    let diff = abs (l - r)
    total + diff)
  0
|> printfn "Answer: %A"
