open System
open System.IO

let leftList, rightList =
  File.ReadLines("data.txt")
  |> Seq.choose (fun line ->
    let items = line.Split(' ') |> Array.filter (fun s -> String.IsNullOrEmpty s |> not)
    
    match items with
    | [| leftListItem; rightListItem |] -> 
      match Int32.TryParse leftListItem, Int32.TryParse rightListItem with
      | (true, l), (true, r) -> Some (l, r)
      | _ -> None
    | _ -> None)
  |> Seq.toList
  |> List.unzip
  
let rightMap = rightList |> List.countBy id |> Map.ofList

leftList
|> List.fold (fun total i -> 
  match Map.tryFind i rightMap with
  | Some count -> (i * count) + total
  | None -> total) 0
|> printfn "Answer: %A"