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
  ||> fun x y -> List.sort x, List.sort y

let rec calculateDistance leftList rightList total =
  match leftList, rightList with
  | [], [] -> total
  | l :: ls, r :: rs ->
    let diff = abs (l - r)
    total + diff |> calculateDistance ls rs
  | _ -> failwith "Lists are not the same length"

calculateDistance leftList rightList 0
|> printfn "Answer: %A"